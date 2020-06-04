;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.http.http-test
  (:require
   [ring.mock.request :refer [request]]
   [juxt.apex.alpha.http.content-negotiation :as conneg]
   [juxt.reap.alpha.api :as reap]
   [juxt.apex.alpha.http.core :as http]
   [clojure.test :refer [deftest is]]
   [juxt.apex.alpha.http.header-names :refer [wrap-headers-normalize-case]]))

(comment
  (let [h (->
           (http/handler
            (reify
              http/ResourceLocator
              (locate-resource [_ uri]
                (if (= (.getPath uri) "/hello.txt")
                  {:apex.http/content "Hello World!"}))
              http/ResponseBody
              (send-ok-response
                  [_ resource response request respond raise]
                (respond
                 (conj response [:body (:apex.http/content resource)])))))
           wrap-headers-normalize-case)]
    (h (request :get "/hello.txt"))))

(defn wrap-dissoc-date [h]
  (fn [req]
    (->
     (h req)
     (update :headers dissoc "date"))))

(deftest locate-resource-test
  (let [h (-> (http/handler
               (reify
                 http/ResourceLocator
                 (locate-resource [_ uri]
                   (when (= (.getPath uri) "/hello.txt")
                     {:apex.http/content "Hello World!"}))
                 http/ResponseBody
                 (send-ok-response
                     [_ resource response request respond raise]
                     (respond
                      (conj response [:body (:apex.http/content resource)])))))
              wrap-dissoc-date)]
    (is (=
         {:status 200
          :headers {}
          :body "Hello World!"}
         (h (request :get "/hello.txt"))))

    (is (=
         {:status 404
          :headers {}}
         (h (request :get "/not-exists"))))))

(deftest content-negotiation-test
  (let [h (->
           (http/handler
            (reify
              http/ResourceLocator
              (locate-resource [_ uri]
                (case (.getPath uri)
                  "/hello"
                  {:apex.http/variants
                   [(java.net.URI. "/hello.html")
                    (java.net.URI. "/hello.txt")]}
                  "/hello.html"
                  {:apex.http/content "<h1>Hello World!</h1>"
                   :apex.http/content-type "text/html;charset=utf-8"}
                  "/hello.txt"
                  {:apex.http/content "Hello World!"
                   :apex.http/content-type "text/plain;charset=utf-8"}
                  ;; else not found
                  nil))

              http/ContentNegotiation
              (best-representation [provider resource request]
                (conneg/select-best-representation
                 request
                 (map #(http/lookup-resource provider %) (:apex.http/variants resource))))

              http/ResponseBody
              (send-ok-response
                  [_ resource response request respond raise]
                  (println "resource is" resource)
                  (respond
                   (conj response [:body (:apex.http/content resource)])))))
           ;; Help with fixed comparisons
           wrap-dissoc-date)]
    (is (=
         {:status 200
          :headers {"content-location" "/hello.html"}
          :body "<h1>Hello World!</h1>"}
         (h (update
             (request :get "/hello")
             :headers conj ["accept" "text/html"]))))))
