;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.alpha.cms.core
  (:require
   [clojure.string :as str]
   [clojure.pprint :refer [pprint]]
   [clojure.xml :as xml]
   [hiccup2.core :refer [html]]
   [hiccup.page :refer [xml-declaration]]
   [juxt.apex.alpha.async.helpers :as a]
   [juxt.apex.alpha.auth-digest.core :refer [wrap-auth-digest]]
   [juxt.apex.alpha.cms.xml :as x]))

(defn uri [req]
  (format "%s://%s%s"
          (-> req :scheme name)
          (-> req :headers (get "host"))
          (-> req :uri)))

(defprotocol ContentStore
  (lookup-resource [_ uri] "Find the resource with the given uri")
  (propfind [_ uri depth] "Find the properties of members of uri"))

;; TODO: Belongs in Apex 'core'
(defprotocol ApexBackend
  (post-resource [_ ctx req respond raise])
  (generate-representation [_ ctx req respond raise]))

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

(defmethod http-method :get [backend {:keys [store] :as ctx} req respond raise]
  (if-let [entity (lookup-resource store (java.net.URI. (uri req)))]
    ;; TODO: Revisit use of 'crux' ns here
    (generate-representation backend (assoc ctx :crux/entity entity) req respond raise)
    (respond {:status 404 :body "Crux CMS: 404 (Not found)\n"})))

(defmethod http-method :head [backend {:keys [store] :as ctx} req respond raise]
  (if-let [entity (lookup-resource store (java.net.URI. (uri req)))]
    (generate-representation
     backend
     (assoc
      ctx
      :crux/entity entity
      :apex/head? true)
     req
     (fn [response]
       (respond (assoc response :body nil)))
     raise)

    (respond {:status 404})))

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
        ent (lookup-resource store uri)
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

(defn make-router [backend {:keys [store vertx engine] :as init-ctx}]
  (assert store)
  (assert vertx)
  (assert engine)
  (->
   (make-handler backend init-ctx)

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
