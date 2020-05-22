(ns juxt.apex.alpha.cms.core
  (:require
   [clojure.xml :as xml]
   [juxt.apex.examples.cms.adoc :as adoc]
   [hiccup.core :refer [html]]
   [hiccup.page :refer [xml-declaration]]
   [juxt.apex.alpha.async.helpers :as a]
   [clojure.pprint :refer [pprint]]
   [ring.middleware.params :refer [wrap-params]]
   [ring.middleware.head :refer [wrap-head]]
   [clojure.java.io :as io]
   [selmer.parser :as selmer]
   [selmer.util :refer [*custom-resource-path*]]
   [clojure.string :as str]))

(defn uri [req]
  (format "%s://%s%s"
          (-> req :scheme name)
          (-> req :headers (get "host"))
          (-> req :uri)))

(defprotocol ContentStore
  (find-entity [_ id] "Find the entity with the given id")
  (propfind [_ uri depth] "Find the properties of members of uri"))

(defn entity-as-html [ent]
  (str "<pre>"
       (->
        (with-out-str (pprint ent))
        (str/replace "<" "&lt;"))
       "</pre>"))

(defn respond-entity [ent req respond raise]
  (respond
   {:status 200
    :headers {"content-type" "text/html"}
    :body (entity-as-html ent)}))

(def templates-source-uri (java.net.URI. "http://localhost:8000/_sources/templates/"))

(defn render-entity-with-selmer-template [ent store engine]
  (binding [*custom-resource-path*
            (. templates-source-uri toURL)]
    (selmer/render-file
     (java.net.URL. (str templates-source-uri (:crux.cms.selmer/template ent)))

     ;; TODO: Does the entity have a crux.cms/source attribute?
     ;; If so, do the adoc dance and extract all the bookmarks are pop them into this entity
     (cond-> ent
       (:template ent) (dissoc :template)

       ;; Merge all the bookmarked content of the adoc source into the template model
       (:crux.cms/source ent)
       (merge (adoc/template-model engine (:crux.cms/content (find-entity store (:crux.cms/source ent))))))

     :custom-resource-path (. templates-source-uri toURL))))

(defn redirect? [ent]
  (when-let [status (:crux.web/status ent)]
    (and (>= status 300) (< status 400))))

(defn rfc1123-date [inst]
  (.
   (java.time.format.DateTimeFormatter/RFC_1123_DATE_TIME)
   format
   inst))

(defmulti method (fn [req respond raise opts] (:request-method req)))

(defmethod method :default [req respond raise opts]
  (respond
   {:status 501}))

(defmethod method :options [req respond raise opts]
  ;; TODO: Check path?
  (respond
   {:status 200
    :headers {"DAV" "1,2"}}))

(defn respond-entity-response [ent req respond raise {:keys [vertx store engine]}]
  (try
    (cond
      (redirect? ent)
      (respond
       {:status (:crux.web/status ent)
        :headers {"location" (str (:crux.web/location ent))}})

      (:crux.cms/file ent)
      (respond
       {:status 200
        :body
        (io/file (:crux.cms/file ent))})

      (string? (:crux.cms/content ent))
      (respond
       {:status 200
        :headers
        (cond-> {}
          (:crux.web/content-type ent)
          (conj ["content-type" (:crux.web/content-type ent)])
          (:crux.web/content-language ent)
          ;; TODO: Support vectors for multiple languages
          (conj ["content-language" (:crux.web/content-language ent)]))
        :body (:crux.cms/content ent)})

      (:crux.cms.selmer/template ent)
      (a/execute-blocking-code
       vertx
       (fn [] (render-entity-with-selmer-template ent store engine))
       {:on-success
        (fn [body]
          (respond
           {:status 200
            :headers
            (cond-> {}
              (:crux.web/content-type ent)
              (conj ["content-type" (:crux.web/content-type ent)])
              (:crux.web/content-language ent)
              ;; TODO: Support vectors for multiple languages (see
              ;; identical TODO above)
              (conj ["content-language" (:crux.web/content-language ent)]))

            :body body}))
        :on-failure
        (fn [t]
          (raise
           (ex-info
            "Failed to render template"
            {:template (:crux.cms.selmer/template ent)} t)))})

      :else
      (respond
       {:status 500
        :body
        (str
         "<body><h2>ERROR - Not handled</h2>"
         (entity-as-html ent)
         "</body>")}))

    (catch Throwable t
      (raise (ex-info (format "Error with path: %s" (:uri req)) {:request req} t))
      )))

(defmethod method :get [req respond raise {:keys [store] :as opts}]
  (let [debug (get-in req [:query-params "debug"])]

    (if-let [ent (find-entity store (java.net.URI. (uri req)))]
      (if debug
        (respond-entity ent req respond raise)
        (respond-entity-response ent req respond raise opts))

      (respond {:status 404 :body "Crux CMS: 404 (Not found)\n"}))))

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

(defmethod method :propfind [req respond raise {:keys [vertx store]}]
  (let [
        ;; "Servers SHOULD treat a request without a Depth header as if a
        ;; "Depth: infinity" header was included." -- RFC 4918
        depth (get-in req [:headers "depth"] "infinity")
        ]

    (let [members (propfind store (java.net.URI. (uri req)) depth)]

      ;; Find which properties are being asked for:
      ;;(pprint (xml/parse (:body req)))

      (respond
       (let [body
             (html
              {:mode :xml}
              (xml-declaration "utf-8")
              [:multistatus {"xmlns" "DAV:"}
               (for [[uri props] members]
                 [:response
                  [:href (str uri)]
                  [:propstat
                   [:prop
                    #_[:displayname "Example collection"]
                    (if (.endsWith (str uri) "/")
                      [:resourcetype
                       [:collection]]
                      [:resourcetype])
                    [:getetag (str (java.util.UUID/randomUUID))]
                    (when (string? (:crux.cms/content props))
                      [:getcontentlength (.length (:crux.cms/content props))])
                    (when (:crux.cms/file props)
                      [:getcontentlength (.length (io/file (:crux.cms/file props)))]
                      )
                    [:getlastmodified (rfc1123-date (java.time.ZonedDateTime/now))]]]])]
              "\n")]

         {:status 207                   ; multi-status
          :headers {"content-type" "application/xml;charset=utf-8"
                    "content-length" (str (.length body))}
          :body body})))))

(defmethod method :lock [req respond raise {:keys [vertx store]}]
  (let [
        ;; "Servers SHOULD treat a request without a Depth header as if a
        ;; "Depth: infinity" header was included." -- RFC 4918
        depth (get-in req [:headers "depth"] "infinity")
        ]

    (pprint (xml/parse (:body req)))

    (respond
     {:status 200}
     #_(let [body
           (html
            {:mode :xml}
            (xml-declaration "utf-8")
            [:multistatus {"xmlns" "DAV:"}
             (for [[uri props] members]
               [:response
                [:href (str uri)]
                [:propstat
                 [:prop
                  #_[:displayname "Example collection"]
                  (if (.endsWith (str uri) "/")
                    [:resourcetype
                     [:collection]]
                    [:resourcetype])
                  [:getetag (str (java.util.UUID/randomUUID))]
                  (when (string? (:crux.cms/content props))
                    [:getcontentlength (.length (:crux.cms/content props))])
                  (when (:crux.cms/file props)
                    [:getcontentlength (.length (io/file (:crux.cms/file props)))]
                    )
                  [:getlastmodified (rfc1123-date (java.time.ZonedDateTime/now))]]]])]
            "\n")]

       {:status 200                   ; multi-status
        :headers {"content-type" "application/xml;charset=utf-8"
                  "content-length" (str (.length body))}
        :body body}))))



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

(defn make-router [{:keys [store vertx engine] :as opts}]
  (assert store)
  (assert vertx)
  (->
   (fn this
     ([req]
      (this req identity (fn [t] (throw t))))
     ([req respond raise]
      (method req respond raise opts)))

   ;; To get the debug query parameter.  Arguably we could use Apex's
   ;; OpenAPI-compatible replacement.
   wrap-params

   ;; This is for strict semantics, but handlers should still check
   ;; the request-method prior to generating expensive bodies.
   wrap-head

   ;; Dev only, removed on production
   (wrap-url-rewrite
    {:canonical {:scheme :https :host-header "juxt.pro"}
     :actual {:scheme :http :host-header "localhost:8000"}})

   wrap-log

   a/wrap-request-body-as-input-stream
   ))
