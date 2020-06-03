;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.examples.cms.cms
  (:require
   [clojure.pprint :refer [pprint]]
   [clojure.string :as str]
   [crux.api :as crux]
   [integrant.core :as ig]
   [juxt.apex.alpha.http.core :as apex]
   [juxt.apex.alpha.vertx.helpers :as a]
   [juxt.apex.alpha.webdav.core :as webdav]
   [juxt.apex.examples.cms.images :as images]
   [juxt.apex.examples.cms.adoc :as adoc]
   [ring.middleware.params :refer [params-request]]
   [selmer.parser :as selmer]
   [selmer.util :refer [*custom-resource-path*]]))

(defn redirect? [resource]
  (when-let [status (:apex.http/status resource)]
    (and (>= status 300) (< status 400))))

;; Copied in this repo - TODO: dedupe!
(defn rfc1123-date [inst]
  (.
   (java.time.format.DateTimeFormatter/RFC_1123_DATE_TIME)
   format
   inst))

(defn binary? [content]
  (re-matches #"\P{Cntrl}*" content))

(defn resource-as-html [ent]
  (let [ent
        (cond-> ent
          (and (:apex.http/content ent)
               (or
                (> (count (:apex.http/content ent)) 200)
                (binary? (:apex.http/content ent))))
          (assoc :apex.http/content (format "<%s bytes of content>" (count (.getBytes (:apex.http/content ent)))))
          )]
    (str "<pre>\n"
         (->
          (with-out-str (pprint ent))
          (str/replace "<" "&lt;"))
         "</pre>\n")))

(defn respond-resource [_ {:keys [apex/resource] :as ctx} req respond raise]
  ;; TODO: Might need authorization to see resource metadata
  ;; (for protected resources)
  (respond
   {:status 200
    :headers {"content-type" "text/html"}
    :body (resource-as-html resource)}))

(def templates-source-uri (java.net.URI. "http://localhost:8000/_sources/templates/"))

(defn generate-body-from-template [backend resource asciidoctor-engine]
  (assert resource)

  (binding [*custom-resource-path*
            (. templates-source-uri toURL)]
    (selmer/render-file
     (java.net.URL. (str templates-source-uri (:apex.selmer/template resource)))

     ;; TODO: Does the entity have a apex/source attribute?
     ;; If so, do the adoc dance and extract all the bookmarks are pop them into this entity
     (cond-> resource
       (:apex.selmer/template resource) (dissoc :template)

       ;; Merge all the bookmarked content of the adoc source into the template model
       (:apex.asciidoctor/source resource)
       (merge
        (adoc/template-model
         asciidoctor-engine
         (:apex.http/content
          (apex/locate-resource backend (:apex.asciidoctor/source resource))))))

     :custom-resource-path (. templates-source-uri toURL))))

(defn respond-resource-response
  "Return the response for a GET request targetting a resource backed by
  a CMS entity."
  [{:keys [vertx engine]} backend {:keys [apex.http/head? apex.http/resource]} req respond raise]

  (assert resource)

  (try
    (cond
      (redirect? resource)
      (respond
       {:status (:apex.http/status resource)
        :headers {"location" (str (:apex.http/location resource))}})

      ;; We are a static representation
      (and (string? (:apex.http/content resource)))
      ;; So our etag is easy to compute
      (respond
       (cond->
           {:status 200
            :headers
            (cond-> {}
              (:apex.http/content-type resource)
              (conj ["content-type" (:apex.http/content-type resource)])

              (:apex.http/content-language resource)
              ;; TODO: Support vectors for multiple languages
              (conj ["content-language" (:apex.http/content-language resource)])

              (:apex.http/content-length resource)
              (conj ["content-length" (str (:apex.http/content-length resource))])

              (:apex.http/last-modified resource)
              (conj ["last-modified"
                     (rfc1123-date
                      (java.time.ZonedDateTime/ofInstant
                       ;; Hmm, representations are last-modified, not resources?
                       (.toInstant (:apex.http/last-modified resource))
                       (java.time.ZoneId/systemDefault)))])

              (:apex.http/entity-tag resource)
              (conj ["etag" (str \" (:apex.http/entity-tag resource) \")]))}

           (not head?)
           (assoc
            :body
            (case (:apex.http/content-coding resource)
              :base64
              (.decode (java.util.Base64/getDecoder) (:apex.http/content resource))
              (:apex.http/content resource)))))

      (:apex.selmer/template resource)
      (let [source-ent (apex/locate-resource backend (:apex.asciidoctor/source resource))
            _ (when-not source-ent
                (throw (ex-info "Expected source entity not found" {:source-entity (:apex.asciidoctor/source resource)})))
            headers
            (cond-> {}
              (:apex.http/content-type resource)
              (conj ["content-type" (:apex.http/content-type resource)])

              (:apex.http/content-language resource)
              ;; TODO: Support vectors for multiple languages (see
              ;; identical TODO above)
              (conj ["content-language" (:apex.http/content-language resource)])

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
                       (.toInstant (:apex.http/last-modified source-ent))
                       (java.time.ZoneId/systemDefault)))])

              ;; No content-length, this will be chunked
              )]
        (if head?
          (respond
           {:status 200
            :headers headers})

          (a/execute-blocking-code
           vertx
           (fn [] (generate-body-from-template backend resource engine))
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
                {:template (:apex.selmer/template resource)}
                t)))})))

      ;; TODO: Refactor me!
      (and (:apex.http/source-image resource) (apex/locate-resource backend (:apex.http/source-image resource)))
      (let [source-resource (apex/locate-resource backend (:apex.http/source-image resource))]
        (case (:apex.http/content-coding source-resource)
          :base64
          (let [baos (new java.io.ByteArrayOutputStream)]
            (images/resize-image
             (new java.io.ByteArrayInputStream (.decode (java.util.Base64/getDecoder) (:apex.http/content source-resource)))
             (get resource :apex.http/width 200)
             baos)
            (let [body (.toByteArray baos)]

              (respond
               {:status 200
                :headers (cond->
                             {"content-length" (str (count body))}

                           (:apex.http/last-modified source-resource)
                           (assoc
                            "last-modified"
                            (rfc1123-date
                             (java.time.ZonedDateTime/ofInstant
                              (.toInstant (:apex.http/last-modified source-resource))
                              (java.time.ZoneId/systemDefault))))

                           (:apex.http/entity-tag source-resource)
                           (assoc "etag" (str \" (:apex.http/entity-tag source-resource) \")))
                ;; Not Ring complaint, but awaiting an adapter from InputStream in my Ring/vertx adapter.
                :body body})))))

      :else
      (respond
       (cond-> {:status 500}
         (not head?)
         (assoc :body
                (str
                 "<body><h2>ERROR - Not handled</h2>"
                 (resource-as-html resource)
                 "</body>")))))

    (catch Throwable t
      (raise (ex-info (format "Error with path: %s" (:uri req)) {:request req} t)))))

;; Promote?

(defn url-rewrite-request [request {:keys [canonical _]}]
  (-> request
      (assoc-in [:headers "host"] (:host-header canonical))
      (assoc :scheme (:scheme canonical))))

(defn url-rewrite-response [response _]
  response)

(defn wrap-url-rewrite [handler opts]
  (fn
    ([request]
     (-> request
         (url-rewrite-request opts)
         handler
         (url-rewrite-response opts)))
    ([request respond raise]
     (handler
      (url-rewrite-request request opts)
      (fn [response] (respond (url-rewrite-response response opts)))
      raise))))

(defn wrap-log [handler]
  (fn
    ([request]
     (println "Incoming sync CMS request:\n" (with-out-str (pprint request)))
     (let [response (handler request)]
       (println "Outgoing CMS response:" (with-out-str (pprint response)))
       response))
    ([request respond raise]
     (println "Incoming async CMS request:\n" (with-out-str (pprint request)))
     (handler
      request
      (fn [response]
        (println "Outgoing CMS response:" (with-out-str (pprint response)))
        (respond response))
      (fn [t]
        (println "Error raised:" t)
        (raise t))))))

(defmethod ig/init-key ::router [_ {:keys [vertx engine crux-node] :as opts}]
  (->
   (apex/handler
    (reify
      apex/ResourceLocator
      (locate-resource [_ uri]
        (crux/entity (crux/db crux-node) uri))

      apex/ResponseBody
      (send-ok-response [this ctx req respond raise]
        ;; To get the debug query parameter.  Arguably we could use Apex's
        ;; OpenAPI-compatible replacement.
        (let [req (params-request req)
              debug (get-in req [:query-params "debug"])]
          (if debug
            (respond-resource this ctx req respond raise)
            (respond-resource-response opts this ctx req respond raise))))

      apex/ResourceUpdate
      (post-resource [_ ctx req respond raise]
        (let [body (slurp (:body req))]
          (crux/submit-tx
           crux-node
           [[:crux.tx/put
             {:crux.db/id (java.net.URI. "https://juxt.pro/frontpage3.css")
              :apex.http/content-type "text/css;charset=utf-8"
              :apex.http/content body
              :apex.http/classification :public}]])
          (respond {:status 201 :body "Uploaded!\n"})))

      apex/ResourceOptions
      (resource-options-headers [_ resource]
        ;; TODO: Not all resources are webdavable, e.g. index.html, so
        ;; don't return a WebDav compliance header in this case.
        {"DAV" (webdav/compliance-value)})

      apex/ServerOptions
      ;; The reason for adding JUXT is to make it easier to search for
      ;; the Apex repo and documentation.
      (server-header [_] "JUXT Apex (Vert.x)")
      (server-options [_] {})

      apex/ReactiveStreaming
      (request-body-as-stream [_ req callback]
        (.
         (:apex.vertx/request req)
         bodyHandler
         (a/h
          (fn [buffer]
            (callback
             (assoc
              req
              :body (new java.io.ByteArrayInputStream (.getBytes buffer))))))))

      webdav/WebDav
      (propfind [this uri depth]
        (let [uris
              (map
               first
               (crux/q
                (crux/db crux-node)
                '{:find [e]
                  :where [(or-join [e] [e :apex.http/content-source] [e :apex.http/content])]}))]
          (into
           {}
           (for [uri
                 (webdav/find-members uri depth uris)]
             [uri (apex/locate-resource this uri)]))))))

   ;; Dev only, removed on production. Definitely a good example of
   ;; middleware.
   (wrap-url-rewrite
    {:canonical {:scheme :https :host-header "juxt.pro"}
     :actual {:scheme :http :host-header "localhost:8000"}})

   ;; Log requests, often optional and sensitive to the logging
   ;; implementation. Definitely middleware.
   ;; wrap-log
   ))
