;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.alpha.http.core
  (:require
   [clojure.string :as str]))

(defn uri [req]
  (format "%s://%s%s"
          (-> req :scheme name)
          (-> req :headers (get "host"))
          (-> req :uri)))

;; TODO: OpenAPI in Apex support should be written in terms of these
;; interfaces.

(defprotocol ResourceLookup
  (lookup-resource [_ uri] "Find the resource with the given uri"))

(defprotocol RepresentationResponse
  (generate-representation [_ ctx req respond raise]))

(defprotocol ResourceUpdate
  (post-resource [_ ctx req respond raise]))

(defprotocol ServerOptions
  (server-options [_]))

(defprotocol ResourceOptions
  (resource-options-headers [_ resource]))

(defprotocol ReactiveStreaming
  (request-body-as-stream [_ req callback]
    "Async streaming adapters only (e.g. Vert.x). Call the callback
    with a Ring-compatible request containing a :body
    InputStream. This must be called in the request thread, otherwise
    the body may have already begun to be read."))

(defn rfc1123-date [inst]
  (.
   (java.time.format.DateTimeFormatter/RFC_1123_DATE_TIME)
   format
   inst))

(defmulti http-method (fn [provider req respond raise] (:request-method req)))

(defmethod http-method :default [provider req respond raise]
  (respond
   {:status 501}))

(defmethod http-method :options [provider request respond raise]
  (cond
    ;; Test me with:
    ;; curl -i --request-target "*" -X OPTIONS http://localhost:8000
    (= (:uri request) "*")
    (respond
     {:status 200
      :headers (server-options provider)})

    :else
    (let [resource (lookup-resource provider (java.net.URI. (uri request)))]
      (respond
       {:status 200
        :headers (resource-options-headers provider resource)}))))

(defmethod http-method :get [provider req respond raise]
  (if-let [resource (lookup-resource provider (java.net.URI. (uri req)))]

    ;; Determine status
    ;; Negotiate content representation
    ;; Compute entity-tag for representation
    ;; Check condition (Last-Modified, If-None-Match)
    ;; Generate response with new entity-tag
    ;; Handle errors (by responding with error response, with appropriate re-negotiation)
    (generate-representation provider {:apex/resource resource} req respond raise)

    (respond {:status 404 :body "Apex: 404 (Not found)\n"})))

(defmethod http-method :head [provider req respond raise]
  (if-let [resource (lookup-resource provider (java.net.URI. (uri req)))]
    (generate-representation
     provider
     {:apex/resource resource
      :apex/head? true}
     req
     (fn [response]
       (respond (assoc response :body nil)))
     raise)

    (respond {:status 404})))

;; POST method
(defmethod http-method :post [provider req respond raise]
  (post-resource provider {} req respond raise))



(defn make-handler [provider]
  (fn handler
    ([req]
     (handler req identity (fn [t] (throw t))))
    ([req respond raise]
     (try
       (http-method provider req respond raise)
       (catch Throwable t
         (raise
          (ex-info
           (format
            "Error on %s on %s"
            (str/upper-case (name (:request-method req)))
            (:uri req))
           {:request req}
           t)))))))
