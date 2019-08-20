(ns juxt.warp.request
  (:require
   [ring.middleware.params :refer [wrap-params]]
   [muuntaja.middleware :as mw]
   [muuntaja.core :as m]
   [juxt.jinx-alpha :as jinx]
   [juxt.warp.format :as format]))

(defn wrap-oas-path [h api]
  (fn [req respond raise]
    (h (merge req {:oas/api api}) respond raise)))

(defn wrap-path-map [h api]
  (fn [req respond raise]
    (let [url (format "%s://%s%s" (name (:scheme req)) (:server-name req) (:uri req))
          servers (->> (get-in api ["servers"]) (map #(get % "url")))
          path (some #(when (.startsWith url %) (subs url (count %))) servers)
          path-item (get-in api ["paths" path])]
      (h (merge req {:oas/url url
                     :oas/servers servers
                     :oas/path path
                     :oas/path-item path-item})
         respond raise))))

(defn wrap-check-404 [h api]
  (fn [req respond raise]
    (let [{:keys [:oas/path :oas/path-item]} req]
      (if
        ;; Not served by server in the 'servers' section
        ;; Or not found in the 'paths' section
        (or (nil? path) (nil? path-item))
        (respond {:status 404 :body "Not Found"})

        ;; Otherwise continue
        (h req respond raise)))))

(defn wrap-check-405 [h]
  (fn [req respond raise]
    (let [path-item (:oas/path-item req)]
      (assert path-item)
      (if-not (get path-item (name (:request-method req)))
        (respond {:status 405 :body "Method Not Allowed"})
        (h req respond raise)))))

(defn wrap-determine-operation [h]
  (fn [req respond raise]
    (let [path-item (:oas/path-item req)
          operation (get path-item (name (:request-method req)))]
      (h (merge req {:oas/operation operation}) respond raise))))

(defn wrap-properties [h options]
  (fn [req respond raise]
    (if-let [f (:properties-fn options)]
      (f (fn [result]
           (h (assoc req ::value result) respond raise)))
      (h req respond raise))))

;; The rule for composing 3-arity Ring middleware is as follows:
;; If you want to call the delegate, simply pass the respond and raise functions along.
;; If you want to return a response map, call the respond function with your response map.
;; If you want to throw an exception, call the raise function with your exception.

(defn wrap-validate-query-params [h]
  (fn [req respond raise]
    (try
      (doseq [{:strs [name in required schema]} (get-in req [:oas/operation "parameters"])
              :when (= in "query")
              :when required]
        (let [entry (find (:query-params req) name)]
          (when (nil? entry)
            (throw (ex-info "400"
                            {:warp.response/status 400
                             :warp.response/body-generator
                             (fn [format-and-charset]
                               (let [message (format "Missing required parameter: %s" name)]
                                 (case (:format format-and-charset)
                                   "text/plain" message
                                   {:error {:message message}})))})))

          ;; TODO: This could be precompiled!
          (let [instance (second entry)
                jinx-schema (jinx/schema schema)
                validation (jinx/validate instance jinx-schema)]

            (when-not (:valid? validation)
              (throw (ex-info
                      "400"
                      {:warp.response/status 400
                       ;; We could have multiple errors here, for now,
                       ;; we're just popping out the first error.
                       ;; Much more work could be done in sending back
                       ;; detailed error traces from jinx.
                       :warp.response/body-generator
                       (fn [format-and-charset]
                         (let [message (format "Query parameter '%s' failed because: %s" name (-> validation :errors first :message))]
                           (case (:format format-and-charset)
                             "text/plain" message
                             {:error
                              {:message message}})))}))))))

      (h req respond raise)

      (catch clojure.lang.ExceptionInfo e
        (h
         (merge
          req
          (select-keys (ex-data e) [:warp.response/status :warp.response/body-generator]))
         respond raise)))))

(def formats (merge (:formats m/default-options)))

(defn wrap-format [h]
  (fn [req respond raise]
    (let [oas-response (:oas/response req)
          content-types (some-> oas-response (get "content") keys)
          formats
          (into {} (concat
                    (for [ct content-types
                          :when (.startsWith ct "text/")]
                      [ct (format/text-format ct)])
                    (select-keys formats content-types)))

          m (m/create (-> m/default-options
                          (assoc :formats formats)
                          (dissoc :default-format)))]
      (try
        (let [req (m/negotiate-and-format-request m req)]
          (h (assoc req :warp/muuntaja-instance m) respond raise))

        (catch clojure.lang.ExceptionInfo e

          ;; If status already >= 400, then just don't negotiate a body response
          (let [status (or (:warp.response/status req) 200)]
            (if (< status 400)

              (h (assoc req
                        :warp/muuntaja-instance (m/create)
                        :warp.response/status 406
                        :warp.response/body {:message "Not Acceptable"
                                             :error (ex-data e)})
                 respond raise)

              ;; RFC 7231 Section 3.4.1:
              ;;            "A user agent cannot rely on proactive negotiation
              ;; preferences being consistently honored, since the origin server
              ;; might not implement proactive negotiation for the requested
              ;; resource or might decide that sending a response that doesn't
              ;; conform to the user agent's preferences is better than sending a
              ;; 406 (Not Acceptable) response."
              ;;
              ;; We feel it's better to send a response that is not
              ;; (strictly) acceptable by the client if we cannot
              ;; negotiate one that is. Let's use the first one.

              (h (assoc req :warp/muuntaja-instance (m/create))
                 respond raise))))))))

;; Let's try not escaping from a 400...

(defn wrap-determine-oas-response
  "The purpose of this middleware is to determine absolutely the OpenAPI
  Response Object of the request. If a status code is already known,
  and this maps directly onto a response (via an explicit code or via
  a range (e.g. 2XX), then we can unambiguously determine the
  response. Otherwise, we may have to choose the response. We cannot
  proceed to call the delegate Ring handler without determining a
  response. In this case, we raise a 500."
  [h]
  (fn [req respond raise]
    (let [status (get req :warp.response/status "default")
          operation (:oas/operation req)]
      (if-let [oas-response (get-in operation ["responses" (str status)])]
        (h
         (assoc req :oas/response oas-response)
         respond raise)
        (raise {:status 500 :body "Unable to determine an OpenAPI response"})))))

(defn wrap-format-response [h]
  (fn [req respond raise]
    (h req
       (fn [response]
         (if-let [m (some-> response :warp/request :warp/muuntaja-instance)]
           (respond (m/format-response m (:warp/request response) response))
           (respond response)))
       raise)))

(defn wrap-generate-response-body []
  (fn [req respond raise]
    ;; Q. Is the content-type available here?
    (let [format-and-charset (:muuntaja/response req)]
      (respond {:status (or (:warp.response/status req) 200)
                :body (or
                       ;; Body can be set already by errors Q.
                       (when-let [bg (:warp.response/body-generator req)]
                         (bg format-and-charset))

                       (when-let [v (::value req)]
                         {:message (format "OK, value is '%s'" (::value req))})

                       ;; TODO: Q. Should we be using :raw-format instead?
                       (case (:format format-and-charset)
                         "text/plain" "OK"
                         "text/html" "<h1>OK</h1>"
                         ;; default
                         {:message "OK"}))

                :warp/request req}))))

(defn wrap-clean-response [h]
  (fn [req respond raise]
    (h req
       (fn [response] (respond (select-keys response [:status :headers :body])))
       raise)))

(defn handler [api options]
  (->
   (wrap-generate-response-body)

   ;; Having determined the status code, we can now do pro-active
   ;; content negotiation since the available content types are a
   ;; function of the status code (in OpenAPI).

   ;; NOTE: There's a bit of a problem here- in that we want to use
   ;; muuntaja to format the response of errors, which may not have
   ;; been generated yet (they're from ring middleware below this
   ;; point!).
   (wrap-format)
   #_(mw/wrap-format (-> muuntaja.core/default-options (dissoc :default-format)))

   (wrap-determine-oas-response)

   ;; We need to determine the parameters, in order to work out the
   ;; status code, which will in turn determine the content
   ;; negotiation.
   (wrap-validate-query-params)
   (wrap-params)

   ;; Get the resource's properties
   (wrap-properties options)

   (wrap-format-response)

   wrap-determine-operation
   wrap-check-405

   (wrap-check-404 api)
   (wrap-path-map api)

   (wrap-oas-path api)

   (wrap-clean-response)))


#_{["findPets" "200"]
 (fn [content-type]
   "Hello World!"
   )}


;; Create a general handler that can be put in Ring, manifold or Pedestal
#_(make-handler [open-api-desc attachments]

              )
