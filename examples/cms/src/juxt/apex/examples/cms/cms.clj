;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.examples.cms.cms
  (:require
   [clojure.pprint :refer [pprint]]
   [clojure.string :as str]
   [crux.api :as crux]
   [integrant.core :as ig]
   [juxt.apex.alpha.async.helpers :as a]
   [juxt.apex.alpha.cms.core :as cms]
   [juxt.apex.alpha.cms.images :as images]
   [juxt.apex.examples.cms.adoc :as adoc]
   [ring.middleware.params :refer [params-request]]
   [selmer.parser :as selmer]
   [selmer.util :refer [*custom-resource-path*]]))

(defn redirect? [ent]
  (when-let [status (:crux.web/status ent)]
    (and (>= status 300) (< status 400))))

;; Copied in this repo - TODO: dedupe!
(defn rfc1123-date [inst]
  (.
   (java.time.format.DateTimeFormatter/RFC_1123_DATE_TIME)
   format
   inst))

(defn binary? [content]
  (re-matches #"\P{Cntrl}*" content))

(defn entity-as-html [ent]
  (let [ent
        (cond-> ent
          (and (:crux.web/content ent)
               (or
                (> (count (:crux.web/content ent)) 200)
                (binary? (:crux.web/content ent))))
          (assoc :crux.web/content (format "<%s bytes of content>" (count (.getBytes (:crux.web/content ent)))))
          )]
    (str "<pre>\n"
         (->
          (with-out-str (pprint ent))
          (str/replace "<" "&lt;"))
         "</pre>\n")))

(defn respond-entity [backend {:keys [crux/entity] :as ctx} req respond raise]
  ;; TODO: Might need authorization to see resource metadata
  ;; (for protected resources)
  (respond
   {:status 200
    :headers {"content-type" "text/html"}
    :body (entity-as-html entity)}))

(def templates-source-uri (java.net.URI. "http://localhost:8000/_sources/templates/"))

(defn render-entity-with-selmer-template [backend entity engine]
  (assert entity)

  (binding [*custom-resource-path*
            (. templates-source-uri toURL)]
    (selmer/render-file
     (java.net.URL. (str templates-source-uri (:crux.cms.selmer/template entity)))

     ;; TODO: Does the entity have a crux.cms/source attribute?
     ;; If so, do the adoc dance and extract all the bookmarks are pop them into this entity
     (cond-> entity
       (:template entity) (dissoc :template)

       ;; Merge all the bookmarked content of the adoc source into the template model
       (:crux.cms/source entity)
       (merge (adoc/template-model engine (:crux.web/content (cms/lookup-resource backend (:crux.cms/source entity))))))

     :custom-resource-path (. templates-source-uri toURL))))

(defn respond-entity-response
  "Return the response for a GET request targetting a resource backed by
  a CMS entity."
  [backend {:keys [vertx engine apex/head? crux/entity]} req respond raise]

  ;; Determine status
  ;; Negotiate content representation
  ;; Compute entity-tag for representation
  ;; Check condition (Last-Modified, If-None-Match)
  ;; Generate response with new entity-tag
  ;; Handle errors (by responding with error response, with appropriate re-negotiation)

  (assert entity)

  (try
    (cond
      (redirect? entity)
      (respond
       {:status (:crux.web/status entity)
        :headers {"location" (str (:crux.web/location entity))}})

      ;; We are a static representation
      (and (string? (:crux.web/content entity)))
      ;; So our etag is easy to compute
      (respond
       (cond->
           {:status 200
            :headers
            (cond-> {}
              (:crux.web/content-type entity)
              (conj ["content-type" (:crux.web/content-type entity)])

              (:crux.web/content-language entity)
              ;; TODO: Support vectors for multiple languages
              (conj ["content-language" (:crux.web/content-language entity)])

              (:crux.web/content-length entity)
              (conj ["content-length" (str (:crux.web/content-length entity))])

              (:crux.web/last-modified entity)
              (conj ["last-modified"
                     (rfc1123-date
                      (java.time.ZonedDateTime/ofInstant
                       (.toInstant (:crux.web/last-modified entity))
                       (java.time.ZoneId/systemDefault)))])

              (:crux.web/entity-tag entity)
              (conj ["etag" (str \" (:crux.web/entity-tag entity) \")]))}

           (not head?)
           (assoc
            :body
            (case (:crux.web/content-coding entity)
              :base64
              (.decode (java.util.Base64/getDecoder) (:crux.web/content entity))
              (:crux.web/content entity)))))

      (:crux.cms.selmer/template entity)
      (let [source-ent (cms/lookup-resource backend (:crux.cms/source entity))
            _ (when-not source-ent
                (throw (ex-info "Expected source entity not found" {:source-entity (:crux.cms/source entity)})))
            headers
            (cond-> {}
              (:crux.web/content-type entity)
              (conj ["content-type" (:crux.web/content-type entity)])

              (:crux.web/content-language entity)
              ;; TODO: Support vectors for multiple languages (see
              ;; identical TODO above)
              (conj ["content-language" (:crux.web/content-language entity)])

              ;; Calc last-modified and/or etag - computed on-the-fly
              ;; in order to prevent stale responses being generated.
              ;;
              ;; TODO: This should return the 'most recent' of the following:
              ;; 1. source data (source-ent) (done!)
              ;; 2. (any dependencies, includes of the source-ent)
              ;; 3. template source
              ;; 4. (any selmer partials included)
              source-ent
              (conj ["last-modified"
                     (rfc1123-date
                      (java.time.ZonedDateTime/ofInstant
                       (.toInstant (:crux.web/last-modified source-ent))
                       (java.time.ZoneId/systemDefault)))])

              ;; No content-length, this will be chunked
              )]
        (if head?
          (respond
           {:status 200
            :headers headers})

          (a/execute-blocking-code
           vertx
           (fn [] (render-entity-with-selmer-template backend entity engine))
           {:on-success
            (fn [body]
              (respond
               {:status 200
                :headers headers
                :body body}))

            :on-failure
            (fn [t]
              (raise
               (ex-info
                "Failed to render template"
                {:template (:crux.cms.selmer/template entity)}
                t)))})))

      ;; TODO: Refactor me!
      (and (:crux.web/source-image entity) (cms/lookup-resource backend (:crux.web/source-image entity)))
      (let [source-ent (cms/lookup-resource backend (:crux.web/source-image entity))]
        (case (:crux.web/content-coding source-ent)
          :base64
          (let [baos (new java.io.ByteArrayOutputStream)]
            (images/resize-image
             (new java.io.ByteArrayInputStream (.decode (java.util.Base64/getDecoder) (:crux.web/content source-ent)))
             (get entity :crux.web/width 200)
             baos)
            (let [body (.toByteArray baos)]

              (respond
               {:status 200
                :headers (cond->
                             {"content-length" (str (count body))}

                           (:crux.web/last-modified source-ent)
                           (assoc
                            "last-modified"
                            (rfc1123-date
                             (java.time.ZonedDateTime/ofInstant
                              (.toInstant (:crux.web/last-modified source-ent))
                              (java.time.ZoneId/systemDefault))))

                           (:crux.web/entity-tag source-ent)
                           (assoc "etag" (str \" (:crux.web/entity-tag source-ent) \")))
                ;; Not Ring complaint, but awaiting an adapter from InputStream in my Ring/vertx adapter.
                :body body})))))

      :else
      (respond
       (cond-> {:status 500}
         (not head?)
         (assoc :body
                (str
                 "<body><h2>ERROR - Not handled</h2>"
                 (entity-as-html entity)
                 "</body>")))))

    (catch Throwable t
      (raise (ex-info (format "Error with path: %s" (:uri req)) {:request req} t)))))

(defmethod ig/init-key ::router [_ {:keys [crux-node] :as opts}]
  (cms/make-router
   (reify
     cms/ApexBackend
     (lookup-resource [_ uri]
       (crux/entity (crux/db crux-node) uri))
     (propfind [this uri depth]
       (let [uris
             (map
              first
              (crux/q
               (crux/db crux-node)
               '{:find [e]
                 :where [(or-join [e] [e :crux.web/content-source] [e :crux.web/content])]}))]
         (into
          {}
          (for [uri
                (cms/find-members uri depth uris)]
            [uri (cms/lookup-resource this uri)]))))


     (post-resource [_ ctx req respond raise]
       (let [body (slurp (:body req))]
         (crux/submit-tx
          crux-node
          [[:crux.tx/put
            {:crux.db/id (java.net.URI. "https://juxt.pro/frontpage3.css")
             :crux.web/content-type "text/css;charset=utf-8"
             :crux.web/content body
             :crux.ac/classification :public}]])
         (respond {:status 201 :body "Uploaded!\n"})))

     (generate-representation [this {:keys [crux/entity] :as ctx} req respond raise]
       ;; To get the debug query parameter.  Arguably we could use Apex's
       ;; OpenAPI-compatible replacement.
       (let [req (params-request req)
             debug (get-in req [:query-params "debug"])]
         (if debug
           (respond-entity this ctx req respond raise)
           (respond-entity-response this ctx req respond raise)))))
   opts))
