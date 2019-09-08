;; Copyright © 2019, JUXT LTD.

(ns juxt.apex.conditional-request-test
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.tools.logging :as log]
   [clojure.test :refer [deftest is testing]]
   [juxt.apex.request :refer [handler]]
   [juxt.apex.yaml :as yaml]
   [juxt.apex.test-util :refer [call-handler]]
   [ring.mock.request :as mock]))

;; See https://developer.mozilla.org/en-US/docs/Web/HTTP/Conditional_requests

(deftest cache-update-test
  (let [api (yaml/parse-string
             (slurp (io/resource "juxt/apex/openapi-examples/petstore.yaml")))
        h (handler
           api
           {:apex/operations
            {"listPets"

             {:apex/action
              (fn [req callback raise]
                (callback
                 (merge
                  req
                  {:apex.response/status 200
                   :apex.response/body
                   [{"name" "Luna"}
                    {"name" "Sven"}]})))

              :apex/validators
              (fn [req callback raise]
                (callback
                 ;; Expectation is to return a new request with
                 ;; validators merged.
                 (merge
                  req
                  {:apex/entity-tag
                   ;; The purpose of this map is also to
                   ;; indicate strong or weak validator strength.
                   {:value "123"}

                   :apex/last-modified
                   {:value (java.time.Instant/parse "2012-12-04T04:21:00Z")}})))}}})

        {:keys [status headers body]}
        @(call-handler h (-> (mock/request
                              :get "http://petstore.swagger.io/v1/pets")))]
    (is (= 200 status))

    (testing "cache-update"

      (let [last-modified-date (get headers "Last-Modified")]

        (testing "last-modified header returned as expected"
          (is (= "Tue, 4 Dec 2012 04:21:00 GMT" last-modified-date)))

        (let [{:keys [status headers body]}
              @(call-handler h (-> (mock/request
                                    :get "http://petstore.swagger.io/v1/pets")
                                   (mock/header "if-modified-since" last-modified-date)
                                   ))]
          (testing "304 if we use the same date in request"
            (is (= 304 status))))

        (let [{:keys [status headers body]}
              @(call-handler h (-> (mock/request
                                    :get "http://petstore.swagger.io/v1/pets")
                                   (mock/header "if-modified-since"
                                                (juxt.apex.request/to-rfc-1123-date-time
                                                 (java.time.Instant/parse "2019-01-01T00:00:00Z")))))]
          (testing "304 if we use a future date in the request"
            (is (= 304 status))))

        (let [{:keys [status headers body]}
              @(call-handler h (-> (mock/request
                                    :get "http://petstore.swagger.io/v1/pets")
                                   (mock/header "if-modified-since"
                                                (juxt.apex.request/to-rfc-1123-date-time
                                                 (java.time.Instant/parse "2010-01-01T00:00:00Z")))))]
          (testing "200 if we use a prior date in the request"
            (is (= 200 status)))

          (testing "last-modified header returned as normal when if-modified-since request header exists"
            (is (= "Tue, 4 Dec 2012 04:21:00 GMT" (get headers "Last-Modified")))))))))
