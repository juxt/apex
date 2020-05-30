;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.alpha.cms.core
  (:require
   [clojure.string :as str]
   [clojure.xml :as xml]
   [hiccup2.core :refer [html]]
   [hiccup.page :refer [xml-declaration]]
   [juxt.apex.alpha.cms.xml :as x]))

(defn uri [req]
  (format "%s://%s%s"
          (-> req :scheme name)
          (-> req :headers (get "host"))
          (-> req :uri)))

;; TODO: Belongs in Apex 'core'
;; TODO: Break up into separate protocols
(defprotocol ApexBackend
  (lookup-resource [_ uri] "Find the resource with the given uri")
  (generate-representation [_ ctx req respond raise])

  ;; TODO: Should belong in a optional 'writeable' protocol
  (post-resource [_ ctx req respond raise])
  ;; TODO: Should belong in an optional 'webdav' protocol
  (propfind [_ uri depth] "Find the properties of members of uri")

  ;; TODO: Should belong to a separate protocol
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

(defmulti http-method (fn [backend req respond raise] (:request-method req)))

(defmethod http-method :default [backend req respond raise]
  (respond
   {:status 501}))

(defmethod http-method :options [backend req respond raise]
  ;; TODO: Check path?
  (respond
   {:status 200
    :headers {"DAV" "1"}}))

(defmethod http-method :get [backend req respond raise]
  (if-let [resource (lookup-resource backend (java.net.URI. (uri req)))]

    ;; Determine status
    ;; Negotiate content representation
    ;; Compute entity-tag for representation
    ;; Check condition (Last-Modified, If-None-Match)
    ;; Generate response with new entity-tag
    ;; Handle errors (by responding with error response, with appropriate re-negotiation)
    (generate-representation backend {:apex/resource resource} req respond raise)

    (respond {:status 404 :body "Apex: 404 (Not found)\n"})))

(defmethod http-method :head [backend req respond raise]
  (if-let [resource (lookup-resource backend (java.net.URI. (uri req)))]
    (generate-representation
     backend
     {:apex/resource resource
      :apex/head? true}
     req
     (fn [response]
       (respond (assoc response :body nil)))
     raise)

    (respond {:status 404})))

;; POST method
(defmethod http-method :post [backend req respond raise]
  (post-resource backend {} req respond raise))

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

(defmethod http-method :propfind [backend req respond raise]

  ;; Unless public, we need to know who is accessing this resource (TODO)

  ;; Do we have an Authorization header?

  ;; We ask the backend to provide the entire body as an input-stream,
  ;; as we determine it's going to be a smallish XML payload.

  (let [cb
        (fn [req]
          (let [body-str (slurp (:body req))
                uri (java.net.URI. (uri req))
                depth (get-in
                       req [:headers "depth"]
                       ;; "Servers SHOULD treat a request without a Depth
                       ;; header as if a "Depth: infinity" header was
                       ;; included." -- RFC 4918
                       "infinity")
                members (propfind backend uri depth)

                props (->>
                       (x/->*
                        {:content [(xml/parse (java.io.ByteArrayInputStream. (.getBytes body-str)))]}
                        :propfind :prop x/content)
                       (map (juxt :tag :content)))
                resource (lookup-resource backend uri)]

            (respond
             (let [body
                   (.toString
                    (html
                     {:mode :xml}
                     (xml-declaration "utf-8")
                     [:multistatus {"xmlns" "DAV:"}
                      (for [[uri ent] members
                            :let [authorized? (= (:apex/classification resource) :public)]]
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
                                 (when-let [etag (:apex.web/entity-tag resource)]
                                   [:getetag etag])

                                 :getcontentlength
                                 (when
                                     (:apex/content resource)
                                     [:getcontentlength (.length (:apex/content resource))])

                                 :getlastmodified
                                 ;; Hmm, not sure it's resources that are 'last modified', more like representations
                                 (when-let [last-modified (:apex/last-modified resource)]
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

               {:status 207             ; multi-status
                :headers {"content-type" "application/xml;charset=utf-8"
                          "content-length" (str (.length body))}
                :body body}))))]

    (if (:body req)
      (cb req)
      (request-body-as-stream backend req cb))))

(defn make-handler [backend]
  (fn handler
    ([req]
     (handler req identity (fn [t] (throw t))))
    ([req respond raise]
     (try
       (http-method backend req respond raise)
       (catch Throwable t
         (raise
          (ex-info
           (format
            "Error on %s on %s"
            (str/upper-case (name (:request-method req)))
            (:uri req))
           {:request req}
           t)))))))
