;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.examples.cms.cms-test
  (:require
   [juxt.apex.alpha.cms.core :as cms]
   [clojure.test :refer [deftest is are testing]])
  (:import
   (io.vertx.reactivex.core Vertx)))


(def entities
  (->>
   [{:crux.db/id (java.net.URI. "https://juxt.pro/A/a/1")
     :crux.cms/content "123"}
    {:crux.db/id (java.net.URI. "https://juxt.pro/A/a/2/i")}
    {:crux.db/id (java.net.URI. "https://juxt.pro/A/b/3")}
    {:crux.db/id (java.net.URI. "https://juxt.pro/B")}
    {:crux.db/id (java.net.URI. "https://juxt.pro/C")}]
   (map (juxt :crux.db/id identity))
   (into {})))

(defrecord TestContentStore []
  cms/ContentStore
  (find-entity [_ id]
    (println "find entity" id)
    (get entities id)))

(deftest get-test
  (let [req
        {:scheme :https
         :headers {"host" "juxt.pro"}
         :request-method :get
         :uri "/A/a/1"}]
    (with-open [vertx (Vertx/vertx)]
      (let [handler
            (cms/make-router
             {:store (->TestContentStore)
              :vertx vertx})
            response (handler req)]
        (is (= 200 (:status response)))
        (is (= "123" (:body response)))))))

(deftest propfind-test
  (let [req
        {:request-method :propfind
         :uri "/dav/"
         :headers
         {"depth" "1",
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
            (cms/make-router
             {:store (->TestContentStore)
              :vertx vertx})
            response (handler req)]
        (is (= 207 (:status response)))
        (is (string? (:body response)))
        ;; TODO: Check xml structure of response body
        ))))
