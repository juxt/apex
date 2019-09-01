(ns juxt.apex.request-test
  (:require
   [clojure.java.io :as io]
   [clojure.tools.logging :as log]
   [clojure.test :refer [deftest is testing]]
   [jsonista.core :as j]
   [juxt.apex.request :refer [handler]]
   [juxt.apex.yaml :as yaml]
   [ring.mock.request :as mock]))

(defn call-handler [handler request]
  (let [p (promise)]
    (handler
     request
     (fn [response] (deliver p response))
     (fn [err] (deliver p err)))
    p))

(deftest responds-with-404-test
  (let [api (yaml/parse-string (slurp (io/resource "juxt/apex/openapi-examples/petstore.yaml")))
        h (handler api {})]
    (is (= 404 (:status @(call-handler h (mock/request :get "http://example.org")))))
    (is (= 404 (:status @(call-handler h (mock/request :get "http://petstore.swagger.io/v1/dummy")))))))

(deftest responds-with-405-test
  (testing "DELETE method is not allowed resulting in a 405 response"
    (let [api (yaml/parse-string (slurp (io/resource "juxt/apex/openapi-examples/petstore.yaml")))
          h (handler api {})
          req (mock/request :delete "http://petstore.swagger.io/v1/pets")]
      (is (= 405 (:status @(call-handler h req)))))))

;; 406 doesn't happen now because we always get a default response
#_(deftest responds-with-406-test
  (let [api (yaml/parse-string (slurp (io/resource "juxt/apex/openapi-examples/petstore.yaml")))
        h (handler api {})]
    (is (= 406 (:status @(call-handler
                          h (-> (mock/request :get "http://petstore.swagger.io/api/pets")
                                (mock/header "accept" "application/yaml"))))))))

;; GET
(deftest get-operation-test
  (let [api (yaml/parse-string (slurp (io/resource "juxt/apex/openapi-examples/petstore.yaml")))
        h (handler api {:operation-handlers
                        {"listPets"
                         (fn [req respond raise]
                           (respond
                            (assoc req
                                   :apex.response/body ["monkey" "cat" "dog" "lizard" "tarantula"])))}})
        response @(call-handler h (mock/request
                                   :get "http://petstore.swagger.io/v1/pets"))]
    (is (= 200 (:status response)))
    (is (= "[\"monkey\",\"cat\",\"dog\",\"lizard\",\"tarantula\"]" (-> response :body slurp)))))


;; Restore when operations tested
#_(deftest coerce-body-to-json-test
    (let [api (yaml/parse-string (slurp (io/resource "juxt/apex/openapi-examples/petstore.yaml")))
          h (handler api {})]
      (let [call (call-handler h
                               (->
                                (mock/request :get "http://petstore.swagger.io/v1/pets")
                                (mock/header "accept" "application/json")))]
        ;; We block until promise is delivered
        (is (= {:status 200,
                :body {"message" "OK - here's a JSON response"},
                :headers {"server" "JUXT apex"
                          "Content-Type" "application/json; charset=utf-8"}}
               (update @call :body j/read-value))))))

;; Restore
#_(deftest simulate-database-property-access-test
  ;; Here, we provide a properties function that the wrap-properties
  ;; middleware will expect.  This properties function simulates a
  ;; fetch of the resource's properties which will be useful in
  ;; pre-determining the various aspects of the resource -
  ;; e.g. existence, last-modified, etc. (see yada for this idea). The
  ;; purpose of this is to avoid calling a potentially expensive
  ;; response generation function in the case where such a call would
  ;; be strictly unnecessary (e.g. the client has indicated via the
  ;; entity-tag that they have a cache of the result).
  (let [get-property
        (fn [value]
          (fn [cb]
            ;; Hand off to another thread!
            (future
              (cb value))))
        api (yaml/parse-string (slurp (io/resource "juxt/apex/openapi-examples/petstore.yaml")))
        h (handler api {:properties-fn (get-property "test")})]
    (let [call (call-handler h
                             (->
                              (mock/request :get "http://petstore.swagger.io/v1/pets")
                              (mock/header "accept" "application/json")))]
      ;; We block until promise is delivered
      (is (= {"message" "OK, value is 'test'"} (:body (update @call :body j/read-value)))))))

;; Restore later
#_(deftest responds-with-400-test
  (testing "Good query parameter causes a 200"
    (let [doc (yaml/parse-string (slurp (io/resource "juxt/apex/tests.yaml")))
          h (handler doc {})]
      (is
       (= 200 (:status @(call-handler h (->
                                         (mock/request :get "https://example.org/api/test-1?foo=ok")
                                         (mock/header "accept" "application/json"))))))))

  (testing "Missing required query parameter causes a 400"
    (let [doc (yaml/parse-string (slurp (io/resource "juxt/apex/tests.yaml")))
          h (handler doc {})]
      (is
       (= 400 (:status @(call-handler h (->
                                         (mock/request :get "https://example.org/api/test-1")
                                         (mock/header "accept" "application/json"))))))))

  (testing "Malformed query parameter causes a 400"
    (let [doc (yaml/parse-string (slurp (io/resource "juxt/apex/tests.yaml")))
          h (handler doc {})]
      (is
       (= 400 (:status @(call-handler h (->
                                         (mock/request :get "https://example.org/api/test-1?foo=toolong")
                                         (mock/header "accept" "application/json")))))))))



;; POST
#_(let [api (yaml/parse-string (slurp (io/resource "juxt/apex/openapi-examples/petstore.yaml")))
      h (handler api {:operation-handlers
                      {"createPets"
                       (fn [req respond raise]
                         (log/trace "Create pets!")
                         (respond req))}})]
  (->
   @(call-handler h (mock/request
                     :post "http://petstore.swagger.io/v1/pets"))
   ))
