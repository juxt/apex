;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.alpha.http.core
  (:require
   [juxt.apex.alpha.http.util :as util]))

;; TODO: OpenAPI in Apex support should be written in terms of these
;; interfaces.

(defprotocol ResourceLocator
  :extend-via-metadata true
  :apex.http/required false
  (locate-resource
    [_ uri]
    "Return the resource identified with the given URI. Return nil if not
    found."))

(defprotocol OkResponse
  :extend-via-metadata true
  :apex.http/required false
  (send-ok-response
    [_ resource response request respond raise]
    "Call the given respond function with a map containing the body and any
    explicit status override and additional headers. The given response argument
    contains pre-determined status and headers."))

(defprotocol ContentNegotiation
  :extend-via-metadata true
  :apex.http/required false
  (best-representation
    [_ resource request]
    "For a given resource, return the resource (or resources) corresponding to
    the best representation (with respect to the request). If a collection
    containing multiple values are returned, a 300 will result. The pattern of
    negotiation is up to the provider (proactive, reactive, transparent,
    etc.)."))

(defprotocol MultipleRepresentations
  :extend-via-metadata true
  :apex.http/required false
  (send-300-response
    [_ representations request respond raise]
    "Satisfy this protocol if you want to support reactive
    negotation."))

(defprotocol LastModified
  :extend-via-metadata true
  :apex.http/required false
  (last-modified
    [_ representation]
    "Return the java.util.Date that the given representation was last modified."))

(defprotocol EntityTag
  :extend-via-metadata true
  :apex.http/required false
  (entity-tag
    [_ representation]
    "Return the current entity-tag for the given representation."))

(defprotocol PostMethod
  :extend-via-metadata true
  :apex.http/required false
  (POST
    [_ ctx request respond raise]
    "Post to the resource."))

(defprotocol ServerOptions
  :extend-via-metadata true
  :apex.http/required false
  (server-header [_]
    "Return the value for server header, or nil to avoid setting it.")
  (server-options [_]))

(defprotocol ResourceOptions
  :extend-via-metadata true
  :apex.http/required false
  (resource-options-headers [_ resource]))

(defprotocol ReactiveStreaming
  :extend-via-metadata true
  :apex.http/required false
  (request-body-as-stream [_ req callback]
    "Async streaming adapters only (e.g. Vert.x). Call the callback
    with a Ring-compatible request containing a :body
    InputStream. This must be called in the request thread, otherwise
    the body may have already begun to be read."))

(defn lookup-resource
  "Return the map corresponding to the resource identified by the given URI. Add
  the URI to the map as the :juxt.http/uri entry. Return nil if not found."
  [provider ^java.net.URI uri]
  (when-let [resource (locate-resource provider uri)]
    (conj resource [:juxt.http/uri uri])))

(defmulti http-method (fn [provider resource request respond raise] (:request-method request)))

(defmethod http-method :default [provider resource request respond raise]
  (respond {:status 501}))

;; TODO: Most :apex ns keywords should be in :juxt.http ns. Refactor!

;;(defn uri? [i] (instance? java.net.URI i))

(defn- GET-or-HEAD [provider resource request respond raise]
  (let [{:juxt.http/keys [variants vary]}
        (if (satisfies? ContentNegotiation provider)
          (best-representation
           provider
           resource request)
          {:juxt.http/variants [resource]})
        representations variants]

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
      (let [representation
            (cond-> representations
              (sequential? representations) first)

            last-modified
            (when (satisfies? LastModified provider)
              (last-modified provider representation))

            entity-tag
            (when (satisfies? EntityTag provider)
              (entity-tag provider representation))

            status 200

            ;; "In theory, the date ought to represent the moment just before
            ;; the payload is generated."
            orig-date
            (new java.util.Date)

            headers
            (cond-> {"date" (util/format-http-date orig-date)}
              last-modified
              (conj ["last-modified" (util/format-http-date last-modified)])
              entity-tag
              (conj ["etag" entity-tag])
              (not= (:juxt.http/uri representation) (:juxt.http/uri resource))
              (conj ["content-location" (str (:juxt.http/uri representation))]))

            response
            {:status status
             :headers headers}]

        ;; TODO: Check condition (Last-Modified, If-None-Match)

        ;; TODO: Handle errors (by responding with error response, with appropriate re-negotiation)

        (cond
          (= (:request-method request) :head)
          (respond (select-keys response [:status :headers]))

          (satisfies? OkResponse provider)
          (send-ok-response provider representation response request respond raise)

          :else
          (throw
           (ex-info
            "Unable to produce response"
            response)))))))

;; Section 4.3.1
(defmethod http-method :get [provider resource request respond raise]
  (GET-or-HEAD provider resource request respond raise))

;; Section 4.3.2
(defmethod http-method :head [provider resource request respond raise]
  (GET-or-HEAD provider resource request respond raise))

;; Section 4.3.3
(defmethod http-method :post [provider resource req respond raise]
  (POST provider {} req respond raise))

;; Section 4.3.4
#_(defmethod http-method :put [provider resource req respond raise]
    )

;; Section 4.3.5
#_(defmethod http-method :delete [provider resource req respond raise]
  )

;; 4.3.7
(defmethod http-method :options [provider resource request respond raise]
  (cond
    ;; Test me with:
    ;; curl -i --request-target "*" -X OPTIONS http://localhost:8000
    (= (:uri request) "*")
    (respond
     {:status 200
      :headers (server-options provider)})

    :else
    (respond
     {:status 200
      :headers (resource-options-headers provider resource)})))
