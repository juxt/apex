;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.alpha.webdav.api
  (:require
   [clojure.pprint :refer [pprint]]
   [clojure.xml :as xml]
   [hiccup.core :refer [html]]
   hiccup.page
   ))

(defmulti method (fn [req respond raise opts] (:request-method req)))

(defmethod method :default [req respond raise opts]
  (respond
   {:status 501}))

(defn mappings
  "depth must be \"1\" or \"infinity\"."
  [collection-path depth candidate-paths]
  (let [collection-path (str collection-path)]
    (filter
     (fn [candidate]
       (let [candidate (str candidate)]
         (when (not= collection-path candidate)
           (re-matches
            (re-pattern
             (format "\\Q%s\\E%s%s/?"
                     collection-path
                     (if-not (.endsWith "/" collection-path) "/" "")
                     (case depth
                       "1" "[^/]+"
                       "infinity" ".+")))
            candidate))))
     candidate-paths)))

(defn rfc1123-date [inst]
  (.
   (java.time.format.DateTimeFormatter/RFC_1123_DATE_TIME)
   format
   inst))

(defmethod method :options [req respond raise opts]
  ;; TODO: Check path?
  (respond
   {:status 200
    :headers {"DAV" "1,2"}}))


(defmethod method :propfind [req respond raise opts]
  (let [
        ;; "Servers SHOULD treat a request without a Depth header as if a
        ;; "Depth: infinity" header was included." -- RFC 4918
        depth (get-in req [:headers "depth"] "infinity")
        ]

    (printf "PROPFIND on path %s, depth is %s\n" (:path opts) (get-in req [:headers "depth"]))

    (pprint (xml/parse (new java.io.ByteArrayInputStream (.getBytes (:body req)))))

    (respond
     (let [body
           (hiccup.core/html
            {:mode :xml}
            (hiccup.page/xml-declaration "utf-8")
            [:multistatus {"xmlns" "DAV:"}
             [:response
              [:href "/dav/"]
              [:propstat
               [:prop
                #_[:displayname "Example collection"]
                [:resourcetype
                 [:collection]]
                [:getetag (str (java.util.UUID/randomUUID))]
                [:getlastmodified (rfc1123-date (java.time.ZonedDateTime/now))]]]]
             [:response
              [:href "/dav/foo"]
              [:propstat
               [:prop
                #_[:displayname "foo"]
                [:resourcetype]
                [:getcontentlength 200]
                [:getetag (str (java.util.UUID/randomUUID))]
                [:getlastmodified (rfc1123-date (java.time.ZonedDateTime/now))]]]]

             [:response
              [:href "/dav/bar"]
              [:propstat
               [:prop
                #_[:displayname "foo"]
                [:resourcetype]
                [:getcontentlength 200]
                [:getetag (str (java.util.UUID/randomUUID))]
                [:getlastmodified (rfc1123-date (java.time.ZonedDateTime/now))]]]]]
            "\n"
            )]

       {:status 207                     ; multi-status
        :headers {"content-type" "application/xml;charset=utf-8"
                  "content-length" (str (.length body))}
        :body body}))))

(defn wrap-log [handler]
  (fn [request respond raise]
    (println "Incoming DAV request:\n" (with-out-str (pprint request)))
    (handler
     request
     (fn [response]
       (println "Outgoing response:" (with-out-str (pprint response)))
       (respond response))
     (fn [t]
       (println "Error raised:" t)
       (raise t)))))

(defn make-handler [opts]
  (->
   (fn
     ([req]
      (method req identity (fn [t] (throw t)) opts))
     ([req respond raise]
      (method req respond raise opts)))
   wrap-log))
