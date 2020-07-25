;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.alpha.http.resource)

;; TODO: OpenAPI in Apex support should be written in terms of these
;; interfaces.

(defprotocol ResourceLocator
  :extend-via-metadata true
  (locate-resource
    [_ uri]
    "Return the resource identified with the given URI. Return nil if not
    found."))

(defprotocol Resource
  :extend-via-metadata true
  (invoke-method
    [_ server resource response request respond raise]
    "Call the given respond function with a map containing the body and any
    explicit status override and additional headers. The given response argument
    contains pre-determined status and headers."))

(defprotocol ContentNegotiation
  :extend-via-metadata true
  (best-representation
    [_ resource request]
    "For a given resource, return the resource (or resources) corresponding to
    the best representation (with respect to the request). If a collection
    containing multiple values are returned, a 300 will result. The pattern of
    negotiation is up to the provider (proactive, reactive, transparent,
    etc.)."))

(defprotocol MultipleRepresentations
  :extend-via-metadata true
  (send-300-response
    [_ representations request respond raise]
    "Satisfy this protocol if you want to support reactive
    negotation."))

(defprotocol LastModified
  :extend-via-metadata true
  (last-modified
    [_ representation]
    "Return the java.util.Date that the given representation was last modified."))

(defprotocol EntityTag
  :extend-via-metadata true
  (entity-tag
    [_ representation]
    "Return the current entity-tag for the given representation."))

(defprotocol ResourceOptions
  :extend-via-metadata true
  (resource-options-headers [_ resource]))

(defprotocol ReactiveStreaming
  :extend-via-metadata true
  (request-body-as-stream [_ req callback]
    "Async streaming adapters only (e.g. Vert.x). Call the callback
    with a Ring-compatible request containing a :body
    InputStream. This must be called in the request thread, otherwise
    the body may have already begun to be read."))
