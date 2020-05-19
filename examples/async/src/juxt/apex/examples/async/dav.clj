(ns juxt.apex.examples.async.dav
  (:require
   [clojure.pprint :refer [pprint]]
   [clojure.xml :as xml]
   [clojure.tools.logging :as log]
   [hiccup.core :refer [html]]
   [juxt.apex.examples.async.async-helpers :as a]))


(defn rfc1123-date [inst]
  (.
   (java.time.format.DateTimeFormatter/RFC_1123_DATE_TIME)
   format
   inst))

(defmulti dav-handler (fn [path req respond raise] (:request-method req)))

(defmethod dav-handler :default [path req respond raise]
  (respond
   {:status 501}))

(defmethod dav-handler :options [path req respond raise]
  (respond
   {:status 200
    :headers {"DAV" "1,2"}}))

(defmethod dav-handler :propfind [path req respond raise]
  (let [
        ;; "Servers SHOULD treat a request without a Depth header as if a
        ;; "Depth: infinity" header was included." -- RFC 4918
        depth (get-in req [:headers "depth"] "infinity")
        vreq (:apex.vertx/request req)]
    (printf "PROPFIND on path %s, depth is %s\n" path depth)
    (.bodyHandler
     vreq
     (a/h
      (fn [buffer]
        (println "Request body")
        #_(println (String. (.getBytes buffer)))
        (pprint (xml/parse (new java.io.ByteArrayInputStream (.getBytes buffer)))))))
    (respond
     (let [body
           (hiccup.core/html
            {:mode :xml}
            (hiccup.page/xml-declaration "utf-8")
            [:multitstatus {"xmlns" "DAV:"}
             [:response
              [:href "/dav/"]
              [:propstat
               [:prop
                #_[:displayname "Example collection"]
                [:resourcetype
                 [:collection]]
                [:getcontentlength 200]
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
                [:getlastmodified (rfc1123-date (java.time.ZonedDateTime/now))]]]]]
            "\n"
            )]

       {:status 207                     ; multi-status
        :headers {"content-type" "application/xml;charset=utf-8"
                  "content-length" (str (.length body))}
        :body body


        }))))



(defn wrap-log [handler]
  (fn [request respond raise]
    (println "Incoming request:\n" (with-out-str (pprint request)))
    (handler
     request
     (fn [response]
       (println "Outgoing response:" (with-out-str (pprint response)))
       (respond response))
     (fn [t]
       (println "Error raised:" t)
       (raise t)))))

(defn router [req respond raise]
  (let [h
        (->
         (fn [req respond raise]
           (condp re-matches (:uri req)

             #"/dav(.*)" :>>
             (fn [[_ path]]
               (dav-handler path req respond raise))

             (do
               (println "Not found:" (:uri req))
               {:status 404})

             ))

         wrap-log)]

    (h req respond raise)))
