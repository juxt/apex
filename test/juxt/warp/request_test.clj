(ns juxt.warp.request-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [jsonista.core :as j]
   [ring.mock.request :as mock]
   [juxt.warp.request :refer [handler]]
   [juxt.warp.yaml :as yaml]))

(defn call-handler [handler request]
  (let [p (promise)]
    (handler
     request
     (fn [response] (deliver p response))
     (fn [err] (deliver p err)))
    p))

(defn get-property [value]
  (fn  [propname cb]
    ;; Hand off to another thread!
    (future
      (cb value))))

(deftest responds-with-404-test
  (let [api (yaml/parse-string (slurp (io/resource "juxt/warp/openapi-examples/petstore-expanded.yaml")))
        h (handler api {})]
    (is (= 404 (:status @(call-handler h (mock/request :get "http://example.org")))))
    (is (= 404 (:status @(call-handler h (mock/request :get "http://petstore.swagger.io/api/dummy")))))))

(deftest responds-with-405-test
  (testing "DELETE method is not allowed resulting in a 405 response"
    (let [api (yaml/parse-string (slurp (io/resource "juxt/warp/openapi-examples/petstore-expanded.yaml")))
          h (handler api {})
          req (mock/request :delete "http://petstore.swagger.io/api/pets")]
      (is (= 405 (:status @(call-handler h req)))))))

(deftest responds-with-406-test
  (let [api (yaml/parse-string (slurp (io/resource "juxt/warp/openapi-examples/petstore-expanded.yaml")))
        h (handler api {:properties-fn (get-property "not-used")})]
    (is (= 406 (:status @(call-handler
                          h (-> (mock/request :get "http://petstore.swagger.io/api/pets")
                                (mock/header "accept" "application/yaml"))))))))

(deftest simulate-database-property-access-test
  ;; Here, we provide a properties function that the wrap-properties
  ;; middleware will expect.  This properties function simulates a
  ;; fetch of the resource's properties which will be useful in
  ;; pre-determining the various aspects of the resource -
  ;; e.g. existence, last-modified, etc. (see yada for this idea). The
  ;; purpose of this is to avoid calling a potentially expensive
  ;; response generation function in the case where such a call would
  ;; be strictly unnecessary (e.g. the client has indicated via the
  ;; entity-tag that they have a cache of the result).
  (let [api (yaml/parse-string (slurp (io/resource "juxt/warp/openapi-examples/petstore-expanded.yaml")))
        h (handler api {:properties-fn (get-property "test")})]
    (let [call (call-handler h
                             (->
                              (mock/request :get "http://petstore.swagger.io/api/pets")
                              (mock/header "accept" "application/json")))]
      ;; We block until promise is delivered
      (is (= {:status 200,
              :body {"message" "value is 'test'"},
              :headers {"Content-Type" "application/json; charset=utf-8"}}
             (update @call :body j/read-value))))))
