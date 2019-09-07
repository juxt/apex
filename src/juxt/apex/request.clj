;; Copyright © 2019, JUXT LTD.

(ns juxt.apex.request
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.tools.logging :as log]
   [juxt.apex.dev :as dev]
   [juxt.apex.redoc :as redoc]
   [juxt.apex.doc :as doc]
   [juxt.apex.format :as format]
   [juxt.jinx-alpha :as jinx]
   [muuntaja.core :as m]
   [ring.middleware.params :refer [wrap-params]]))

(defn wrap-oas-api [h api]
  (fn [req respond raise]
    (h (merge req {:oas/api api}) respond raise)))

(defn wrap-path-map [h api {:juxt.apex.dev/keys [additional-servers]}]
  (fn [req respond raise]
    (let [url (format "%s://%s%s" (name (:scheme req)) (get-in req [:headers "host"]) (:uri req))
          servers (->> (get-in api ["servers"])
                       (concat additional-servers)
                       (map #(get % "url")))

          request-path (some #(when (.startsWith url %) (subs url (count %))) servers)]


      (if-let [path-info (and request-path
                              (some
                               (fn [[path path-item]]
                                 (let [matcher (or (:apex/matcher (meta path-item))
                                                   (doc/compile-path-template path))]
                                   (when-let [params (matcher request-path)]
                                     {:oas/path path
                                      :oas/path-item path-item
                                      :oas/operation (get path-item (name (:request-method req)))
                                      :apex.request/path-params params})))
                               (get api "paths")))]

        (h (merge
            req
            {:oas/url url
             :oas/servers servers
             :apex/request-path request-path}
            path-info)
           respond raise)

        ;; This is a 404
        ;; TODO: Do conneg
        (try
          (respond
           {:status 404
            :headers
            {"content-type" "text/html;charset=utf-8"}
            :body
            (dev/process-content
             (slurp (io/resource "juxt/apex/404.html"))
             {:status 404 :title "Not Found"}
             api)})
          (catch Exception e (raise e)))))))

(defn wrap-check-405 [h]
  (fn [req respond raise]
    (let [path-item (:oas/path-item req)]
      (assert path-item)
      (if-not (get path-item (name (:request-method req)))
        (respond {:status 405 :body "Method Not Allowed"})
        (h req respond raise)))))

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
                            {:apex.response/status 400
                             :apex.response/body-generator
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
                      {:apex.response/status 400
                       ;; We could have multiple errors here, for now,
                       ;; we're just popping out the first error.
                       ;; Much more work could be done in sending back
                       ;; detailed error traces from jinx.
                       :apex.response/body-generator
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
          (select-keys (ex-data e) [:apex.response/status :apex.response/body-generator]))
         respond raise)))))

(def formats (merge (:formats m/default-options)))

(defn wrap-format
  ([h]
   (wrap-format h {}))
  ([h {:keys [default-format]}]
   (fn [req respond raise]
     (let [oas-response (:oas/response req)
           content-types (some-> oas-response (get "content") keys)
           formats
           (into {} (concat
                     (for [ct content-types
                           :when (.startsWith ct "text/")]
                       [ct (format/text-format ct)])
                     (select-keys formats content-types)))

           m (m/create (cond-> m/default-options
                         true (assoc :formats formats)
                         default-format (assoc :default-format default-format)))]
       (try

         ;; TODO: Shouldn't this be m/request-format only?
         (let [req (m/negotiate-and-format-request m req)]
           (h (assoc req :apex/muuntaja-instance m) respond raise))

         (catch clojure.lang.ExceptionInfo e

           ;; If status already >= 400, then just don't negotiate a body response
           (let [status (or (:apex.response/status req) 200)]
             (if (< status 400)

               (h (assoc req
                         :apex/muuntaja-instance (m/create)
                         :apex.response/status 406
                         :apex.response/body-generator
                         (fn [format-and-charset]
                           {:message "Not Acceptable"
                            :error (ex-data e)}))
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

               (h (assoc req :apex/muuntaja-instance (m/create))
                  respond raise)))))))))

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
    (let [status (get req :apex.response/status)
          operation (:oas/operation req)]
      (if-let [oas-response (get-in operation ["responses" (str status)]
                                    (get-in operation ["responses" "default"]))]
        (h
         (assoc req :oas/response oas-response)
         respond raise)
        (raise (ex-info "Unable to determine an OpenAPI response" {}))))))

(defn wrap-format-response [h]
  (fn [req respond raise]
    (h req
       (fn [response]
         (if-let [m (some-> response :apex/request :apex/muuntaja-instance)]
           (respond (m/format-response m (:apex/request response) response))
           (respond response)))
       raise)))

(defn wrap-generate-response-body []
  (fn [req respond raise]
    (let [format-and-charset (:muuntaja/response req)]

      (respond
       (merge
        (when-let [headers (:apex.response/headers req)] {:headers headers})
        {:status (:apex.response/status req)

         :body (or
                (when-let [body (:apex.response/body req)]
                  body)

                (when-let [bg (:apex.response/body-generator req)]
                  (bg format-and-charset))

                ;; Deprecated
                (when-let [v (::value req)]
                  {:message (format "OK, value is '%s'" (::value req))}))

         :apex/request req})))))

(defn wrap-clean-response [h]
  (fn [req respond raise]
    (h req
       (fn [response] (respond (select-keys response [:status :headers :body])))
       raise)))

(defn operation-handler [req opts]
  (let [operation (:oas/operation req)
        opId (get operation "operationId")]
    (get-in opts [:apex/operations opId :apex/action])))

(defmulti http-method (fn [req callback raise opts] (:request-method req)))

(defmethod http-method :get [req callback raise opts]
  ;; We'll throw an error to help the user know they should add a
  ;; operation function for this operation.
  (if-let [operation-handler (operation-handler req opts)]
    (operation-handler
     req
     (fn [req] (callback
                (merge
                 ;; if not set, default to a 200
                 {:apex.response/status 200}
                 req)))
     raise)

    ;; Error, no operation handler
    (let [op-id (get-in req [:oas/operation "operationId"])]
      (raise (ex-info
              (format "No operation defined for %s" op-id)
              {:error-code ::no-operation
               :operation-id op-id
               })))))

(defmethod http-method :post [req callback raise opts]
  (if-let [operation-handler (operation-handler req opts)]
    (operation-handler
     req
     (fn [req] (callback req))
     raise)

    (let [op-id (get-in req [:oas/operation "operationId"])]
      (raise (ex-info
              (format "No operation defined for %s" op-id)
              {:operation-id op-id
               ;; TODO: Set a code here such that this can be rendered
               ;; nicely by the juxt.apex.dev ns to explain exactly
               ;; what the user should do, with appropriate
               ;; documentation.
               })))))

(defn wrap-execute-method [h options]
  (fn [req respond raise]
    (let [operation (:oas/operation req)
          opId (get operation "operationId")
          operation-handler (get-in options [:apex/operations opId :apex/action])]
      (http-method
       req
       (fn [req]
         (h req respond raise))
       raise
       options))))

(defmulti validator->header (fn [[k v]] k))

(defmethod validator->header :apex/entity-tag
  [[_ v]] ["ETag" (:value v)])

(defn to-rfc-1123-date-time [instant]
  (.format java.time.format.DateTimeFormatter/RFC_1123_DATE_TIME
           (java.time.ZonedDateTime/ofInstant instant (java.time.ZoneId/of "GMT"))))

(defn from-rfc-1123-date-time [s]
  (some->> s
           (.parse java.time.format.DateTimeFormatter/RFC_1123_DATE_TIME)
           (java.time.Instant/from)))

(defmethod validator->header :apex/last-modified
  [[_ v]]
  ["Last-Modified" (to-rfc-1123-date-time (:value v))])

(defn since? [instant since]
  (= (.compareTo instant since) 1))

(defn wrap-conditional-request [h options]
  (fn [req respond raise]
    (let [operation (:oas/operation req)
          opId (get operation "operationId")]
      (if-let [fv (get-in options [:apex/operations opId :apex/validators])]
        (fv
         req
         (fn [req]
           (let [since (from-rfc-1123-date-time (get-in req [:headers "if-modified-since"]))
                 modified (:value (:apex/last-modified req))]

             (if (and since modified (not (since? modified since)))
               (respond {:status 304})
               (h req
                  (fn [response]
                    (respond (update response :headers merge (into {} (map validator->header (select-keys req [:apex/entity-tag :apex/last-modified]))))))
                  raise))))
         raise)
        ;; No validators
        (h req respond raise)))))

(defn wrap-server-header [h]
  (fn [req respond raise]
    (h req (fn [response]
             (respond (assoc-in response [:headers "server"] "JUXT Apex")))
       raise)))

;; TODO: Refactoring: Rename keyword ns :oas to :apex.oas

(defn wrap-internal-error [h api]
  (fn [req respond raise]
    (h
     req
     respond
     (fn [error]
       (log/trace "wrap-internal-error2")
       (log/trace "keys of req are" (keys req))
       (log/trace "operation" (:oas/operation req))
       (let [responses (get-in req [:oas/operation "responses"])
             response (get responses "500" (get responses "default"))
             produces (keys (get response "content"))
             m (m/create (-> (update m/default-options :formats select-keys produces)))]

         (respond
          (m/format-response
           m
           req {:status 500
                :body {:error error}})))))))

(defn handler [api options]
  (->
   ;; TODO: We should be careful not to overuse the Ring middleware
   ;; higher-order function concept. It's OK for Ring middleware to be
   ;; coarse-grained, for performance and coherence reasons. Multiple
   ;; Ring middleware that have implicit dependency relationships
   ;; should be combined where appropriate.
   (wrap-generate-response-body)

   ;; Having determined the status code, we can now do pro-active
   ;; content negotiation since the available content types are a
   ;; function of the status code (in OpenAPI).

   ;; NOTE: There's a bit of a problem here- in that we want to use
   ;; muuntaja to format the response of errors, which may not have
   ;; been generated yet (they're from ring middleware below this
   ;; point!).
   (wrap-format options)

   (wrap-determine-oas-response)

   (wrap-execute-method options)

   ;; Conditional requests
   (wrap-conditional-request options)

   ;; We need to determine the parameters, in order to work out the
   ;; status code, which will in turn determine the content
   ;; negotiation.
   (wrap-validate-query-params)
   (wrap-params)

   ;; Get the resource's properties
   (wrap-properties options)

   (wrap-format-response)

   wrap-check-405

   ;; Developer only feature that uses knowledge in the API to
   ;; generate a set of paths. This should be useful for JSON too.

   ;; Note, this isn't right because it triggers on both a missing
   ;; route and a missing pet. Each of these cases need a different
   ;; error message. Needs more thinking.
   ;;(dev/wrap-helpful-404 api)
   (dev/wrap-helpful-error api)

   ;;(wrap-internal-error api)

   (wrap-path-map api options)

   ;; NOTE: All error handlers from now cannot rely on wrap-path-map being run

   (wrap-oas-api api)

   (wrap-server-header)

   (wrap-clean-response)

;;   (redoc/wrap-redoc api options)
   ))
