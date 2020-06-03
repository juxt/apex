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

(defprotocol ^:apex/required ResourceLocator
  (locate-resource
    [_ uri]
    "Find the resource with the given uri."))

(defprotocol ^:apex/required ResponseBody
  (send-ok-response
    [_ ctx request respond raise]
    "Call the given respond function with a map containing the body and any
    explicit status override and additional headers."))

(defprotocol ^:apex/optional ContentNegotiation
  (negotiate-content
    [_ resource request]
    "For a given resource, return the resource (or resources) corresponding to
    the best representation (with respect to the request). If a collection
    containing multiple values are returned, a 300 will result. The pattern of
    negotiation is up to the provider (proactive, reactive, transparent,
    etc.). If the resource has a URI, return the URI rather than the resource,
    so it can be subsequently located and placed in the 'Content-Location'
    response header. Optional."))

(defprotocol ^:apex/optional MultipleRepresentations
  (send-300-response
    [_ representations request respond raise]
    "Satisfy this protocol if you want to support reactive
    negotationn. Optional."))

(defprotocol ^:apex/optional ResourceUpdate
  (post-resource
    [_ ctx request respond raise]
    "Update the resource to the new state."))

(defprotocol ^:apex/optional ServerOptions
  (server-header [_]
    "Return the value for server header, or nil to avoid setting it.")
  (server-options [_]))

(defprotocol ^:apex/optional ResourceOptions
  (resource-options-headers [_ resource]))

(defprotocol ^:apex/optional ReactiveStreaming
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

;;(defn uri? [i] (instance? java.net.URI i))

(defn get-or-head-method [provider request respond raise]
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
        (if (satisfies? MultipleRepresentations provider)
          (send-300-response provider (filter uri? representations) request respond raise)
          (throw (ex-info "negotiate-content of juxt.apex.alpha.http.ContentNegotiation protocol returned multiple representations but provider does not satisfy juxt.apex.alpha.http.MultipleRepresentations protocol"
                          {})))

        :else
        (let [representation-maybe-uri
              (cond-> representations
                (sequential? representations) first)

              [representation content-location]
              (if (instance? java.net.URI representation-maybe-uri)
                [(locate-resource provider representation-maybe-uri) representation-maybe-uri]
                [representation-maybe-uri])

              status 200

              headers
              (cond-> {}
                content-location (conj ["content-location" content-location]))]

          ;; TODO: Generate response with new entity-tag

          ;; TODO: Check condition (Last-Modified, If-None-Match)

          ;; TODO: Handle errors (by responding with error response, with appropriate re-negotiation)

          (if (= (:request-method request) :head)
            (respond {:status status
                      :headers headers})
            (send-ok-response
             provider
             {:status status
              :headers headers
              :apex/resource resource
              :apex/representation representation}
             request respond raise)))))

    (respond {:status 404 :body "Apex: 404 (Not found)\n"})))

;; Section 4.3.1
(defmethod http-method :get [provider request respond raise]
  (get-or-head-method provider request respond raise))

;; Section 4.3.2
(defmethod http-method :head [provider request respond raise]
  (get-or-head-method provider request respond raise))

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
  (when-not
      (satisfies? ResourceLocator provider)
      (throw
       (ex-info
        "Provider must satisfy mandatory ResourceLocator protocol"
        {:provider provider
         :protocol ResourceLocator})))
  (when-not
      (satisfies? ResponseBody provider)
      (throw
       (ex-info
        "Provider must satisfy mandatory ResponseBody protocol"
        {:provider provider
         :protocol ResponseBody})))
  (fn handler
    ([req]
     (handler req identity (fn [t] (throw t))))
    ([req respond raise]
     (try
       (http-method
        provider
        req
        (fn [response]
          (let [server (when
                           (satisfies? ServerOptions provider)
                           (server-header provider))]
            (respond (cond-> response server (assoc-in [:headers "server"] server)))))
        raise)
       (catch Throwable t
         (raise
          (ex-info
           (format
            "Error on %s of %s"
            (str/upper-case (name (:request-method req)))
            (:uri req))
           {:request req}
           t)))))))
