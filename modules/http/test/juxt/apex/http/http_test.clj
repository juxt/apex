;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.http.http-test
  (:require
   [ring.mock.request :refer [request]]
   [juxt.apex.alpha.http.core :as http]
   [clojure.test :refer [deftest is]]
   [juxt.apex.alpha.http.header-names :refer [wrap-headers-normalize-case]]))

(comment
  (let [h (->
           (http/handler
            (reify
              http/ResourceLocator
              (locate-resource [_ uri]
                (when (= (.getPath uri) "/hello.txt")
                  {:apex.http/content "Hello World!"}))
              http/Resource
              (invoke-method
                  [_ resource response request respond raise]
                  (case (:request-method request)
                    :head (respond response)
                    :get (respond
                          (conj response [:body (:apex.http/content resource)]))))))
           wrap-headers-normalize-case)]
    (h (request :get "/hello.txt"))))
