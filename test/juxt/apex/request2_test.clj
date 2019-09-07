;; Copyright © 2019, JUXT LTD.

(ns juxt.apex.request2-test
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.tools.logging :as log]
   [clojure.test :refer [deftest is testing]]
   [jsonista.core :as j]
   [juxt.apex.request2 :refer [openapi-handler]]
   [juxt.apex.test-util :refer [call-handler]]
   [juxt.apex.yaml :as yaml]
   [ring.mock.request :as mock]))


(deftest app-test
  (let [doc (yaml/parse-string (slurp (io/resource "juxt/apex/openapi-examples/petstore.yaml")))
        app (openapi-handler doc)]

    (testing "OK"
      (is (= 200 (:status @(call-handler app {:request-method :get :uri "/pets"})))))

    (testing "Not found"
      (is (= 404 (:status @(call-handler app {:request-method :get :uri "/pets2"})))))

    (testing "No such method"
      (is (= 405 (:status @(call-handler app {:request-method :delete :uri "/pets"})))))

    (testing "OPTIONS"
      (let [{:keys [status headers]} @(call-handler app {:request-method :options :uri "/pets"})]
        (is (= 200 status))
        (is (= {"Allow" "GET,POST,OPTIONS"} headers))))))


(let [doc (yaml/parse-string (slurp (io/resource "juxt/apex/openapi-examples/petstore.yaml")))
      app (openapi-handler doc)]

  @(call-handler app {:request-method :get :uri "/pets"}))
