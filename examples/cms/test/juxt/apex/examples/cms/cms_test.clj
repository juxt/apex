;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.examples.cms.cms-test
  (:require
   [juxt.apex.alpha.http.core :as apex]
   [juxt.apex.alpha.webdav.core :as webdav]
   [clojure.test :refer [deftest is]])
  (:import
   (io.vertx.reactivex.core Vertx)))

(def entities
  (->>
   [{:crux.db/id (java.net.URI. "https://juxt.pro/A/a/1")
     :crux.web/content "123"}
    {:crux.db/id (java.net.URI. "https://juxt.pro/A/a/2/i")}
    {:crux.db/id (java.net.URI. "https://juxt.pro/A/b/3")}
    {:crux.db/id (java.net.URI. "https://juxt.pro/B")}
    {:crux.db/id (java.net.URI. "https://juxt.pro/C")}]
   (map (juxt :crux.db/id identity))
   (into {})))

(defrecord TestProvider [store vertx]
  apex/ResourceLocator
  (locate-resource [_ uri]
    (get entities uri))
  webdav/WebDav
  (propfind [this uri depth]
    (into {}
          (for [uri
                (webdav/find-members uri depth (keys entities))]
            [uri (apex/locate-resource this uri)]))))

(deftest get-test
  (let [req
        {:request-method :get
         :scheme :https
         :uri "/A/a/1"
         :headers {"host" "juxt.pro"}
         }]
    (with-open [vertx (Vertx/vertx)]
      (let [handler
            (apex/make-handler
             (->TestProvider nil vertx))
            response (handler req)]
        (is (= 200 (:status response)))
        (is (= "123" (:body response)))))))

(deftest propfind-test
  (let [req
        {:request-method :propfind
         :scheme :https
         :uri "/A/"
         :headers
         {"host" "juxt.pro"
          "depth" "1"
          "content-type" "application/xml"}
         :body
         (new java.io.ByteArrayInputStream
              (.getBytes
               "<propfind xmlns=\"DAV:\">
                 <prop>
                  <resourcetype/>
                  <getcontentlength/>
                  <getetag/>
                  <getlastmodified/>
                 </prop>
                </propfind>"))}]
    (with-open [vertx (Vertx/vertx)]
      (let [handler
            (apex/make-handler
             (->TestProvider nil vertx))
            response (handler req)]
        (is (= 207 (:status response)))
        (is (string? (:body response)))

        ;; TODO: Check that it returns
        ))))
