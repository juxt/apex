;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.alpha.http.core
  (:require
   [juxt.apex.alpha.http.util :as util]
   [juxt.apex.alpha.http.resource
    :refer [locate-resource
            ContentNegotiation best-representation
            MultipleRepresentations send-300-response
            LastModified last-modified
            EntityTag entity-tag
            Resource invoke-method
            resource-options-headers]]
   [juxt.apex.alpha.http.server
    :refer [server-options]]))

(defn lookup-resource
  "Return the map corresponding to the resource identified by the given URI. Add
  the URI to the map as the :juxt.http/uri entry. Return nil if not found."
  [provider ^java.net.URI uri]
  (when-let [resource (locate-resource provider uri)]
    (conj resource [:juxt.http/uri uri])))

(defmulti http-method (fn [resource-provider server-provider resource request respond raise] (:request-method request)))

(defmethod http-method :default [resource-provider server-provider resource request respond raise]
  (respond {:status 501}))

;; TODO: Most :apex ns keywords should be in :juxt.http ns. Refactor!

;;(defn uri? [i] (instance? java.net.URI i))

(defn- GET-or-HEAD [resource-provider server-provider resource request respond raise]
  (let [{:juxt.http/keys [variants vary]}
        (if (satisfies? ContentNegotiation resource-provider)
          (best-representation
           resource-provider
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
      (if (satisfies? MultipleRepresentations resource-provider)
        (send-300-response resource-provider (filter uri? representations) request respond raise)
        (throw (ex-info "negotiate-content of juxt.apex.alpha.http.ContentNegotiation protocol returned multiple representations but resource-provider does not satisfy juxt.apex.alpha.http.MultipleRepresentations protocol"
                        {})))

      :else
      (let [representation
            (cond-> representations
              (sequential? representations) first)

            last-modified
            (when (satisfies? LastModified resource-provider)
              (last-modified resource-provider representation))

            entity-tag
            (when (satisfies? EntityTag resource-provider)
              (entity-tag resource-provider representation))

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
          (satisfies? Resource resource-provider)
          (invoke-method resource-provider server-provider representation response request respond raise)
          :else (respond response))))))

;; Section 4.3.1
(defmethod http-method :get [resource-provider server-provider resource request respond raise]
  (GET-or-HEAD resource-provider server-provider resource request respond raise))

;; Section 4.3.2
(defmethod http-method :head [resource-provider server-provider resource request respond raise]
  (GET-or-HEAD resource-provider server-provider resource request respond raise))

;; Section 4.3.3
(defmethod http-method :post [resource-provider server-provider resource request respond raise]
  (invoke-method resource-provider server-provider resource {:status 201} request respond raise))

;; Section 4.3.4
(defmethod http-method :put [resource-provider server-provider resource request respond raise]
  (invoke-method resource-provider server-provider resource {} request respond raise))

;; Section 4.3.5
(defmethod http-method :delete [resource-provider server-provider resource request respond raise]
  (invoke-method resource-provider server-provider resource {} request respond raise))

;; 4.3.7
(defmethod http-method :options [resource-provider server-provider resource request respond raise]
  (cond
    ;; Test me with:
    ;; curl -i --request-target "*" -X OPTIONS http://localhost:8000
    (= (:uri request) "*")
    (respond
     {:status 200
      :headers (server-options server-provider)})

    :else
    (respond
     {:status 200
      :headers (resource-options-headers resource-provider resource)})))
