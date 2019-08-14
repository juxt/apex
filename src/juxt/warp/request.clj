(ns juxt.warp.request
  (:require
   [muuntaja.middleware :as mw]))

(defn wrap-catch-negotiate-error [h]
  (fn [req respond raise]
    (try
      (h req respond raise)
      (catch clojure.lang.ExceptionInfo e
        (if (#{:muuntaja/response-format-negotiation} (:type (ex-data e)))
          {:status 406
           :body "Not Acceptable"}
          (raise e))))))

(defn wrap-oas-path [h api]
  (fn [req respond raise]
    (let [url (format "%s://%s%s" (name (:scheme req)) (:server-name req) (:uri req))
          servers (->> (get-in api ["servers"]) (map #(get % "url")))
          path (some #(when (.startsWith url %) (subs url (count %))) servers)]
      (cond
        (nil? path)
        ;; Not served by server in the 'servers' section
        (respond {:status 404 :body "Not Found"})

        :else
        (let [path-item (get-in api ["paths" path])]
          (cond
            (nil? path-item)
            ;; Not found in the 'paths' section
            (respond {:status 404 :body "Not Found"})

            :else
            (respond
             (h (merge req {:oas/api api
                            :oas/url url
                            :oas/servers servers
                            :oas/path path
                            :oas/path-item path-item})
                respond raise))))))))

(defn wrap-check-405 [h]
  (fn [req respond raise]
    (let [path-item (:oas/path-item req)
          methods (set (keys path-item))]
      (cond
        (not (contains? methods (name (:request-method req))))
        (respond {:status 405 :body "Method Not Allowed"})
        :else (respond (h (merge req {:oas/methods methods}) respond raise))))))

(defn wrap-properties [h options]
  (fn [req respond raise]
    (if-let [f (:properties-fn options)]
      (f "foo" (fn [result]
                 (respond (h (assoc req ::value result) respond raise))))
      (throw (ex-info "No properties fn!" {})))))

(defn handler [api options]
  (->
   (fn [req respond raise]

     (respond {:status 200 :body "OK"})

     #_(throw (ex-info "TODO" {:request req})) #_{:url url
                                                :req req
                                                :servers servers
                                                ;;     :path-item path-item
                                                :path path
                                                :methods methods})

   ;; Get the resource's properties
   (wrap-properties options)

   (mw/wrap-format (dissoc muuntaja.core/default-options :default-format))
   wrap-catch-negotiate-error

   wrap-check-405
   (wrap-oas-path api)))


#_{["findPets" "200"]
 (fn [content-type]
   "Hello World!"
   )}


;; Create a general handler that can be put in Ring, manifold or Pedestal
#_(make-handler [open-api-desc attachments]

              )
