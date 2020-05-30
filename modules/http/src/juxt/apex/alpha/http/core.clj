;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.alpha.http.core
  (:require
   [clojure.string :as str]))

(defn uri [req]
  (format "%s://%s%s"
          (-> req :scheme name)
          (-> req :headers (get "host"))
          (-> req :uri)))

;; TODO: Belongs in Apex 'core'
;; TODO: Break up into separate protocols
(defprotocol ApexBackend
  (lookup-resource [_ uri] "Find the resource with the given uri")
  (generate-representation [_ ctx req respond raise])

  ;; TODO: Should belong in a optional 'writeable' protocol
  (post-resource [_ ctx req respond raise])

  ;; TODO: Should belong to a separate protocol
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

(defmulti http-method (fn [backend req respond raise] (:request-method req)))

(defmethod http-method :default [backend req respond raise]
  (respond
   {:status 501}))

(defmethod http-method :options [backend req respond raise]
  ;; TODO: Check path?
  (respond
   {:status 200
    :headers {"DAV" "1"}}))

(defmethod http-method :get [backend req respond raise]
  (if-let [resource (lookup-resource backend (java.net.URI. (uri req)))]

    ;; Determine status
    ;; Negotiate content representation
    ;; Compute entity-tag for representation
    ;; Check condition (Last-Modified, If-None-Match)
    ;; Generate response with new entity-tag
    ;; Handle errors (by responding with error response, with appropriate re-negotiation)
    (generate-representation backend {:apex/resource resource} req respond raise)

    (respond {:status 404 :body "Apex: 404 (Not found)\n"})))

(defmethod http-method :head [backend req respond raise]
  (if-let [resource (lookup-resource backend (java.net.URI. (uri req)))]
    (generate-representation
     backend
     {:apex/resource resource
      :apex/head? true}
     req
     (fn [response]
       (respond (assoc response :body nil)))
     raise)

    (respond {:status 404})))

;; POST method
(defmethod http-method :post [backend req respond raise]
  (post-resource backend {} req respond raise))



(defn make-handler [backend]
  (fn handler
    ([req]
     (handler req identity (fn [t] (throw t))))
    ([req respond raise]
     (try
       (http-method backend req respond raise)
       (catch Throwable t
         (raise
          (ex-info
           (format
            "Error on %s on %s"
            (str/upper-case (name (:request-method req)))
            (:uri req))
           {:request req}
           t)))))))
