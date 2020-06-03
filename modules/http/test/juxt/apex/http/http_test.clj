;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.http.http-test
  (:require
   [ring.mock.request :refer [request]]
   [juxt.apex.alpha.http.core :as http]
   [clojure.test :refer [deftest is]]))

(defrecord BasicProvider []
  http/ResourceLocator
  (locate-resource
    [_ uri]
    (if (= (.getPath uri) "/hello.txt")
      {}))
  http/ResponseBody
  (send-ok-response [_ ctx request respond raise]
    (respond ctx)))

(deftest basic-test
  (let [h (http/make-handler (->BasicProvider))]
    (let [{:keys [status]} (h (request :get "/hello.txt"))]
      (is (= 200 status)))))
