(ns juxt.warp.request
  (:require
   [muuntaja.middleware :as mw]))


(defn wrap-catch-negotiate-error [h]
  (fn [req]
    (try
      (h req)
      (catch clojure.lang.ExceptionInfo e
        (if (#{:muuntaja/response-format-negotiation} (:type (ex-data e)))
          {:status 406
           :body "Not Acceptable"}
          (throw e))))))

(defn wrap-oas-path [h api]
  (fn [req]
    (let [url (format "%s://%s%s" (name (:scheme req)) (:server-name req) (:uri req))
          servers (->> (get-in api ["servers"]) (map #(get % "url")))
          path (some #(when (.startsWith url %) (subs url (count %))) servers)]
      (cond
        (nil? path)
        ;; Not served by server in the 'servers' section
        {:status 404 :body "Not Found"}

        :else
        (let [path-item (get-in api ["paths" path])]
          (cond
            (nil? path-item)
            ;; Not found in the 'paths' section
            {:status 404 :body "Not Found"}

            :else
            (h (merge req {:oas/url url
                           :oas/servers servers
                           :oas/path path
                           :oas/path-item path-item}))))))))

(defn wrap-check-405 [h]
  (fn [req]
    (let [path-item (:oas/path-item req)
          methods (set (keys path-item))]
      (cond
        (not (contains? methods (name (:request-method req))))
        {:status 405 :body "Method Not Allowed"}
        :else (h (merge req {:oas/methods methods}))))))

(defn handler [api]
  (->
   (fn [req]

     (throw (ex-info "TODO" {:request req})) #_{:url url
                                                :req req
                                                :servers servers
                                                ;;     :path-item path-item
                                                :path path
                                                :methods methods})

   (mw/wrap-format (dissoc muuntaja.core/default-options :default-format))
   wrap-catch-negotiate-error

   wrap-check-405
   (wrap-oas-path api)


   ))


#_{["findPets" "200"]
 (fn [content-type]
   "Hello World!"
   )}


;; Create a general handler that can be put in Ring, manifold or Pedestal
#_(make-handler [open-api-desc attachments]

              )
