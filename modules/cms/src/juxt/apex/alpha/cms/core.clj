;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.alpha.cms.core
  (:require
   [clojure.java.io :as io]
   [clojure.pprint :refer [pprint]]
   [clojure.string :as str]
   [clojure.xml :as xml]
   [hiccup2.core :refer [html]]
   [hiccup.page :refer [xml-declaration]]
   [juxt.apex.alpha.async.helpers :as a]
   [juxt.apex.alpha.cms.images :as images]
   [juxt.apex.alpha.auth-digest.core :refer [wrap-auth-digest]]
   [juxt.apex.alpha.cms.xml :as x]
   [juxt.apex.examples.cms.adoc :as adoc]
   [ring.middleware.head :refer [head-response]]
   [ring.middleware.params :refer [params-request]]
   [selmer.parser :as selmer]
   [selmer.util :refer [*custom-resource-path*]]
   ))

(defn uri [req]
  (format "%s://%s%s"
          (-> req :scheme name)
          (-> req :headers (get "host"))
          (-> req :uri)))

(defprotocol ContentStore
  (find-entity [_ id] "Find the entity with the given id")
  (propfind [_ uri depth] "Find the properties of members of uri"))

;; TODO: Belongs in Apex 'core'
(defprotocol ApexBackend
  (post-resource [_ ctx req respond raise]))

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

(defn respond-entity [{:keys [crux/entity] :as ctx} req respond raise]
  ;; TODO: Might need authorization to see resource metadata
  ;; (for protected resources)
  (respond
   {:status 200
    :headers {"content-type" "text/html"}
    :body (entity-as-html entity)}))

(def templates-source-uri (java.net.URI. "http://localhost:8000/_sources/templates/"))

(defn render-entity-with-selmer-template [entity store engine]
  (println "render-entity-with-selmer-template")
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
       (merge (adoc/template-model engine (:crux.web/content (find-entity store (:crux.cms/source entity))))))

     :custom-resource-path (. templates-source-uri toURL))))

(defn redirect? [ent]
  (when-let [status (:crux.web/status ent)]
    (and (>= status 300) (< status 400))))

(defn rfc1123-date [inst]
  (.
   (java.time.format.DateTimeFormatter/RFC_1123_DATE_TIME)
   format
   inst))

(defmulti http-method (fn [backend ctx req respond raise] (:request-method req)))

(defmethod http-method :default [backend ctx req respond raise]
  (respond
   {:status 501}))

(defmethod http-method :options [backend ctx req respond raise]
  ;; TODO: Check path?
  (respond
   {:status 200
    :headers {"DAV" "1"}}))

(defn respond-entity-response
  "Return the response for a GET request targetting a resource backed by
  a CMS entity."
  [{:keys [vertx store engine apex/head? crux/entity]} req respond raise]

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
      (let [source-ent (find-entity store (:crux.cms/source entity))
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
           (fn [] (render-entity-with-selmer-template entity store engine))
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
      (and (:crux.web/source-image entity) (find-entity store (:crux.web/source-image entity)))
      (let [source-ent (find-entity store (:crux.web/source-image entity))]
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

(defmethod http-method :get [backend {:keys [store] :as ctx} req respond raise]
  ;; To get the debug query parameter.  Arguably we could use Apex's
  ;; OpenAPI-compatible replacement.

  (assert store)

  (let [req (params-request req)
        debug (get-in req [:query-params "debug"])]

    (if-let [ent (find-entity store (java.net.URI. (uri req)))]
      (if debug
        (respond-entity (assoc ctx :crux-entity ent) req respond raise)
        (respond-entity-response (assoc ctx :crux/entity ent) req respond raise))

      (respond {:status 404 :body "Crux CMS: 404 (Not found)\n"}))))

(defmethod http-method :head [backend {:keys [store] :as ctx} req respond raise]
  (if-let [ent (find-entity store (java.net.URI. (uri req)))]
    (respond-entity-response
     (assoc ctx
            :apex/head? true
            :crux/entity ent) req respond raise)

    (respond {:status 404 :body "Crux CMS: 404 (Not found)\n"})))

;; POST method
(defmethod http-method :post [backend {:keys [callback] :as ctx} req respond raise]
  (post-resource backend ctx req respond raise))

;; PROPFIND method

(defn find-members [uri depth candidates]
  (reduce
   (fn [acc candidate]
     (if (.startsWith (str candidate) (str uri))
       (let [segments (str/split (subs (str candidate) (.length (str uri))) #"/")]
         (case depth
           "0" (conj acc uri)
           "1" (conj acc uri (java.net.URI. (str uri (first segments) (if (next segments) "/" ""))))
           "infinity"
           (loop [path ""
                  segments segments
                  acc acc]
             (if-let [segment (first segments)]
               (let [path (str path segment (if (next segments) "/" ""))]
                 (recur
                  path
                  (next segments)
                  (conj acc (java.net.URI. (str uri path)))))
               acc))))
       acc))
   #{}
   candidates))

(defmethod http-method :propfind [{:keys [vertx store]} req respond raise]
  (let [
        ;; "Servers SHOULD treat a request without a Depth header as if a
        ;; "Depth: infinity" header was included." -- RFC 4918
        depth (get-in req [:headers "depth"] "infinity")
        uri (java.net.URI. (uri req))
        ent (find-entity store uri)
        ]

    ;; Unless public, we need to know who is accessing this resource (TODO)

    ;; Do we have an Authorization header?

    (let [members (propfind store uri depth)
          body-str (slurp (:body req))
          props (->>
                 (x/->*
                  {:content [(xml/parse (java.io.ByteArrayInputStream. (.getBytes body-str)))]}
                  :propfind :prop x/content)
                 (map (juxt :tag :content)))]

      (respond
       (let [body
             (.toString
              (html
               {:mode :xml}
               (xml-declaration "utf-8")
               [:multistatus {"xmlns" "DAV:"}
                (for [[uri ent] members
                      :let [authorized? (= (:crux.ac/classification ent) :public)]]
                  (when true
                    [:response
                     [:href (str uri)]
                     [:propstat
                      [:prop
                       #_[:displayname "Example collection"]
                       (for [[prop-name prop-content] props]
                         (case prop-name
                           :resourcetype
                           (if (.endsWith (str uri) "/")
                             [:resourcetype
                              [:collection]]
                             [:resourcetype])

                           :getetag
                           (when-let [etag (:crux.web/entity-tag ent)]
                             [:getetag etag])

                           :getcontentlength
                           (when
                               (:crux.web/content ent)
                               [:getcontentlength (.length (:crux.web/content ent))])

                           :getlastmodified
                           (when-let [last-modified (:crux.web/last-modified ent)]
                             [:getlastmodified
                              (rfc1123-date
                               (java.time.ZonedDateTime/ofInstant
                                (.toInstant last-modified)
                                (java.time.ZoneId/systemDefault)))])

                           ;; Anything else, ignore
                           nil))]
                      [:status
                       (if true #_authorized?
                           "HTTP/1.1 200 OK"
                           "HTTP/1.1 401 Unauthorized")]]]))]
               "\n"))]

         {:status 207                   ; multi-status
          :headers {"content-type" "application/xml;charset=utf-8"
                    "content-length" (str (.length body))}
          :body body})))))

;; Middleware

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

(defn make-handler [backend init-ctx]
  (fn handler
    ([req]
     (handler req identity (fn [t] (throw t))))
    ([req respond raise]
     (try
       (http-method backend init-ctx req respond raise)
       (catch Throwable t
         (raise
          (ex-info
           (format
            "Error on %s on %s"
            (str/upper-case (name (:request-method req)))
            (:uri req))
           {:request req}
           t)))))))

(defn wrap-head
  "Middleware that sets the response body to nil."
  {:added "1.1"}
  [handler]
  (fn
    ([request]
     (head-response (handler request)))
    ([request respond raise]
     (handler
      request
      (fn [response]
        (respond (head-response response request)))
      raise))))

(defn make-router [backend {:keys [store vertx engine] :as init-ctx}]
  (assert store)
  (assert vertx)
  (assert engine)
  (->
   (make-handler backend init-ctx)

   ;; This is for strict semantics, but handlers should still check
   ;; the request-method prior to generating expensive bodies.
   wrap-head

   ;; Digest authentication. Clients are not allowed to use basic auth
   ;; over insecure http.
   wrap-auth-digest

   ;; Dev only, removed on production. Definitely a good example of
   ;; middleware.
   (wrap-url-rewrite
    {:canonical {:scheme :https :host-header "juxt.pro"}
     :actual {:scheme :http :host-header "localhost:8000"}})

   ;; Log requests, often optional and sensitive to the logging
   ;; implementation. Definitely middleware.
   ;; wrap-log

   ;; Prime the Ring request with a blocking stream
   a/wrap-read-all-request-body))
