;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.http.http-test
  (:require
   [ring.mock.request :refer [request]]
   [juxt.apex.alpha.http.core :as http]
   [clojure.test :refer [deftest is]]))

(comment
  (let [h (http/handler
           (reify
             http/ResourceLocator
             (locate-resource [_ uri]
               (if (= (.getPath uri) "/hello.txt")
                 {:apex.http/content "Hello World!"}))))]
    (h (request :get "/hello.txt"))))

(deftest basic-test
  (let [h (http/handler
           (reify
             http/ResourceLocator
             (locate-resource [_ uri]
               (if (= (.getPath uri) "/hello.txt")
                 {:apex.http/content "Hello World!"}))))]
    (is (=
         {:status 200
          :headers {}
          :body "Hello World!"}
         (h (request :get "/hello.txt"))))

    (is (=
         {:status 404
          :headers {}}
         (h (request :get "/not-exists"))))))



#_http/ResponseBody
  #_(send-ok-response [_ ctx request respond raise]
    (respond ctx))
