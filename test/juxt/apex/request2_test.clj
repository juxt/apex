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

(def pets [{:type "cat"}
           {:type "dog"}
           {:type "goat"}
           {:type "goldfish"}
           {:type "lizard"}])

(let
    [doc (yaml/parse-string
          (slurp
           (io/resource "juxt/apex/openapi-examples/petstore.yaml")))
     app (openapi-handler
          doc
          {:apex/add-implicit-head? true
           :apex/resources
           {"/pets"

            {:apex/validators
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
                  {:value (java.time.Instant/parse "2012-12-04T04:21:00Z")}})))

             :apex/methods
             {:get
              {:handler
               (fn [req respond raise]
                 (respond {:status 200 :body pets}))}}}}})]

  @(call-handler app {:request-method :head :uri "/pets"})
  )

(deftest app-test
  (let [doc (yaml/parse-string
             (slurp
              (io/resource "juxt/apex/openapi-examples/petstore.yaml")))
        app (openapi-handler
             doc
             {:apex/resources
              {"/pets"

               {:apex/validators
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
                     {:value (java.time.Instant/parse "2012-12-04T04:21:00Z")}})))

                :apex/methods
                {:get
                 {:handler
                  (fn [req respond raise]
                    (respond {:status 200 :body pets}))}}}}})]

    (testing "OK"
      (let [{:keys [status body]} @(call-handler app {:request-method :get :uri "/pets"})]
        (is (= 200 status))
        (is (= pets body))))

    (testing "Not found"
      (let [{:keys [status]}
            @(call-handler
              app
              {:request-method :get
               :uri "/pets2"})]
        (is (= 404 status))))

    (testing "No such method"
      (let [{:keys [status]} @(call-handler
                               app
                               {:request-method :delete
                                :uri "/pets"})]
        (is (= 405 status))))

    (testing "OPTIONS"
      (let [{:keys [status headers]}
            @(call-handler
              app
              {:request-method :options
               :uri "/pets"})]
        (is (= 200 status))
        (is (= {"Allow" "GET,HEAD,POST,OPTIONS"} headers))))

    (testing "HEAD"
      (let [head-response
            @(call-handler
              app
              {:request-method :head
               :uri "/pets"})
            get-response
            @(call-handler
              app
              {:request-method :get
               :uri "/pets"})]
        (testing "HEAD is successful"
          (is (= 200 (:status head-response))))
        (testing "GET and HEAD have identical headers"
          (is (= (:headers get-response) (:headers head-response))))
        (testing "Body of HEAD is nil"
          (is (nil? (:body head-response)))
          ;; And not just because the GET body is nil
          (is (not (nil? (:body get-response)))))))

    (testing "Conditional requests"

      (let [{:keys [status headers body]}
            @(call-handler
              app
              {:request-method :get
               :uri "/pets"})]

        (testing "cache-update"

          (let [last-modified-date (get headers "Last-Modified")]

            (testing "last-modified header returned as expected"
              (is (= "Tue, 4 Dec 2012 04:21:00 GMT" last-modified-date)))

            (let [{:keys [status headers body]}
                  @(call-handler app {:request-method :get
                                      :uri "/pets"
                                      :headers {"if-modified-since" last-modified-date}})]

              (testing "304 if we use the same date in request"
                (is (= 304 status))))

            (let [{:keys [status headers body]}
                  @(call-handler app {:request-method :get
                                      :uri "/pets"
                                      :headers {"if-modified-since"
                                                (juxt.apex.request/to-rfc-1123-date-time
                                                 (java.time.Instant/parse "2019-01-01T00:00:00Z"))}})]

              (testing "304 if we use a future date in the request"
                (is (= 304 status))))

            (let [{:keys [status headers body]}
                  @(call-handler app {:request-method :get
                                      :uri "/pets"
                                      :headers {"if-modified-since"
                                                (juxt.apex.request/to-rfc-1123-date-time
                                                 (java.time.Instant/parse "2010-01-01T00:00:00Z"))}})]

              (testing "200 if we use a prior date in the request"
                (is (= 200 status)))

              (testing "last-modified header returned as normal when if-modified-since request header exists"
                (is (= "Tue, 4 Dec 2012 04:21:00 GMT" (get headers "Last-Modified")))))))))))
