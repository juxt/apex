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

(defprotocol ResourceLocator
  (locate-resource
    [_ uri]
    "Find the resource with the given uri. Required."))

(defprotocol ContentNegotiation
  (negotiate-content
    [_ resource request]
    "For a given resource, return the resource (or resources)
    corresponding to the best representation (with respect to the
    request). If a collection containing multiple values are returned,
    a 300 will result. The pattern of negotiation is up to the
    provider (proactive, reactive, transparent, etc.). If the resource
    has a URI, return the URI rather than the resource, so it can be
    subsequently located and placed in the 'Content-Location' response
    header. Optional."))

(defprotocol RepresentationResponse
  (generate-representation
    [_ ctx request respond raise]))

(defprotocol ResourceUpdate
  (post-resource
    [_ ctx request respond raise]))

(defprotocol ServerOptions
  (server-header [_]
    "Return the value for server header, or nil to avoid setting it.")
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

;; TODO: Most :apex ns keywords should be in :apex.http ns. Refactor!

;; Section 4.3.1
(defmethod http-method :get [provider request respond raise]
  (if-let [resource (locate-resource provider (java.net.URI. (uri request)))]

    ;; Determine status: 200 (or 206, partial content)

    (let [conneg? (satisfies? ContentNegotiation provider)
          representations
          (if conneg?
            (negotiate-content provider resource request)
            resource)]

      (cond
        (or
         (nil? representations)
         (and (sequential? representations)
              (zero? (count representations))))
        (respond {:status 406})

        (and (sequential? representations)
             (>= (count representations) 2))
        (respond {:status 300
                  :body "TODO: Render multiple representations"})

        :else
        (let [content-resource-uri
              (cond-> representations
                (sequential? representations) first)

              ]

          ;; Generate response with new entity-tag
          ;; Handle errors (by responding with error response, with appropriate re-negotiation)
          (generate-representation
           provider
           {:apex/resource resource
            }
           request respond raise)

          ;; Check condition (Last-Modified, If-None-Match)
          )))



    (respond {:status 404 :body "Apex: 404 (Not found)\n"})))

;; Section 4.3.2
(defmethod http-method :head [provider req respond raise]
  (if-let [resource (locate-resource provider (java.net.URI. (uri req)))]
    (generate-representation
     provider
     {:apex/resource resource
      :apex/head? true}
     req
     (fn [response]
       (respond (assoc response :body nil)))
     raise)

    (respond {:status 404})))

;; Section 4.3.3
(defmethod http-method :post [provider req respond raise]
  (post-resource provider {} req respond raise))

;; Section 4.3.4
#_(defmethod http-method :put [provider req respond raise]
    )

;; Section 4.3.5
#_(defmethod http-method :delete [provider req respond raise]
  )

;; 4.3.7
(defmethod http-method :options [provider request respond raise]
  (cond
    ;; Test me with:
    ;; curl -i --request-target "*" -X OPTIONS http://localhost:8000
    (= (:uri request) "*")
    (respond
     {:status 200
      :headers (server-options provider)})

    :else
    (let [resource (locate-resource provider (java.net.URI. (uri request)))]
      (respond
       {:status 200
        :headers (resource-options-headers provider resource)}))))

(defn make-handler [provider]
  (fn handler
    ([req]
     (handler req identity (fn [t] (throw t))))
    ([req respond raise]
     (try
       (http-method
        provider
        req
        (fn [response]
          (let [server (server-header provider)]
            (respond (cond-> response server (assoc-in [:headers "server"] server)))))
        raise)
       (catch Throwable t
         (raise
          (ex-info
           (format
            "Error on %s on %s"
            (str/upper-case (name (:request-method req)))
            (:uri req))
           {:request req}
           t)))))))
