;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.alpha.webdav.core
  (:require
   [juxt.apex.alpha.http.handler :refer [request-url]]
   [clojure.string :as str]
   [clojure.xml :as xml]
   [hiccup2.core :refer [html]]
   [hiccup.page :refer [xml-declaration]]
   [juxt.apex.alpha.http.core :as http]
   [juxt.apex.alpha.http.resource :as resource]
   [juxt.apex.alpha.http.util :as util]
   [juxt.apex.alpha.webdav.xml :as x]))

(defprotocol WebDav
  (propfind [_ uri depth] "Find the properties of members of uri"))

;; TODO: Not yet properly implemented.
(defn compliance-value
  "Determine the compliance classes supported by the provider and return
  as a string that can be used in the DAV header of an OPTIONS
  response."
  []
  ;; Hard-code for now
  "1")

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

(defmethod http/http-method :propfind [backend req respond raise]

  ;; Unless public, we need to know who is accessing this resource (TODO)

  ;; Do we have an Authorization header?

  ;; We ask the backend to provide the entire body as an input-stream,
  ;; as we determine it's going to be a smallish XML payload.

  (let [cb
        (fn [req]
          (let [body-str (slurp (:body req))
                uri (java.net.URI. (request-url req))
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
                resource (http/lookup-resource backend uri)]

            (respond
             (let [body
                   (.toString
                    (html
                     {:mode :xml}
                     (xml-declaration "utf-8")
                     [:multistatus {"xmlns" "DAV:"}
                      (for [[uri ent] members
                            :let [authorized? (= (:http/classification resource) :public)]]
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
                                     (:juxt.http/content resource)
                                     [:getcontentlength (.length (:juxt.http/content resource))])

                                 :getlastmodified
                                 ;; Hmm, not sure it's resources that are 'last modified', more like representations
                                 (when-let [last-modified (:juxt.http/last-modified resource)]
                                   [:getlastmodified
                                    (util/format-http-date last-modified)])

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
      (resource/request-body-as-stream backend req cb))))
