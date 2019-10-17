;; Copyright © 2019, JUXT LTD.

(ns juxt.apex.alpha2.openapi-test
  (:require
   [clojure.java.io :as io]
   [clojure.pprint :refer [pprint]]
   [clojure.test :refer [deftest is testing use-fixtures]]
   [clojure.tools.logging :as log]
   [juxt.apex.alpha2.openapi :refer [compile-handler]]
   [juxt.apex.alpha2.util :refer [to-rfc-1123-date-time]]
   [juxt.apex.yaml :as yaml]
   [ring.mock.request :as mock]))

;; The outer handler, for testing Apex middleware.
(def ^:dynamic *app*)

;; An atom that can store state to compare against expectations.
(def ^:dynamic *results*)

(def database
  (atom {"1" {"name" "Sven" "tag" "Dog"}
         "2" {"name" "Luna" "tag" "Cat"}
         "3" {"name" "Arya" "tag" "Cat"}
         "4" {"name" "Kaia" "tag" "Cat"}
         "5" {"name" "Vega" "tag" "Dog"}}))

(defn collate-results [h]
  (fn
    ([req]
     (let [response (h req)]
       (swap! *results* assoc :request req :response response)
       response))
    ([req respond raise]
     (h req
        (fn [response]
          (swap! *results* assoc :request req :response response)
          (respond response))
        raise))))

(defn constant-handler [response]
  (fn
    ([req] (if (fn? response) (response req) response))
    ([req respond raise]
     (respond (if (fn? response) (response req) response)))))

(defn test-handler []
  (let [doc (yaml/parse-string
             (slurp
              (io/resource "juxt/apex/openapi-examples/petstore-expanded.yaml")))]
    (compile-handler
     doc
     {:apex/add-implicit-head? true
      :apex/handler-middleware-transform (fn [_ mw] (conj mw collate-results))
      :apex/resources
      {"/pets"
       {:apex/methods
        {:get
         {:handler
          (constant-handler {:status 200
                             :body (vals @database)})}

         :post
         {:handler
          (constant-handler {:status 201
                             :body "OK"})}}

        :apex/validators
        (fn
          [req callback raise]
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
             {:value (java.time.Instant/parse "2012-12-04T04:21:00Z")}})))}

       "/pets/{id}"
       {:apex/methods
        {:get
         {:handler
          (constant-handler
           (fn [req]
             (let [id (get-in req [:path-params :id])
                   pet (get @database id)]
               (if pet
                 {:status 200 :body pet}
                 {:status 404}))))}}}}})))

(defn sync-request [request]
  (assert *app*)
  (*app* request))

(defn async-request [request]
  (assert *app*)
  (let [p (promise)]
    (*app*
     request
     (fn [response] (deliver p response))
     (fn [err] (deliver p err)))
    @p))

(defn request [request]
  (let [async-response (async-request request)
        sync-response (sync-request request)]
    (if (not= async-response sync-response)
      (throw (ex-info "Async and Sync requests do not return the same data!"))
      async-response)))

(use-fixtures :once
  (fn [f]
    (binding [*app* (test-handler)
              *results* (atom {})]
      (f))))

(deftest happy-path-test
  (testing "GET /pets is OK and returns pets"
    (let [{:keys [status body]}
          (request {:request-method :get :uri "/pets"})]
      (is (= 200 status))
      (is (= (vals @database) body))))
  (testing "Get pet by id is OK"
    (let [{:keys [status body]}
          (request {:request-method :get :uri "/pets/2"})]
      (is (= 200 status))
      (is (= (get @database "1") {"name" "Sven" "tag" "Dog"})))))

#_(binding [*app* (test-handler)
          *results* (atom {})]
  (request {:request-method :post :uri "/pets" :body (jsonista.core/write-value-as-string {"id" "10" "name" "Rex" "type" "Dog"})})
  @*results*
  )


;; TODO: Write up request2_test.adoc to explain the motivations behind
;; this ns.

(defn ppr-str [x]
  (with-out-str (pprint x)))

(deftest param-test
  (testing "Path parameters are present and coerced to expected types"
    (request {:request-method :get :uri "/pets/2"})
    (is (= {:id "2"} (-> @*results* :request :path-params)))
    (is (-> @*results* :request :apex/parameters))))

(deftest not-found-test
  (testing "Not found"
    (let [{:keys [status]}
          (request
           {:request-method :get
            :uri "/pets2"})]
      (is (= 404 status)))))

;; Reitit is able to return a 405 if the correct URL is chosen but
;; with the wrong method.
(deftest no-such-method-test
  (testing "No such method"
    (let [{:keys [status]}
          (request
           {:request-method :delete
            :uri "/pets"})]
      (is (= 405 status)))))

(deftest options-test
  (testing "OPTIONS"
    (let [{:keys [status headers]}
          (request
           {:request-method :options
            :uri "/pets"})]
      (is (= 200 status))
      (is (= {"Allow" "GET,HEAD,POST,OPTIONS"} headers)))))

(deftest head-test
  (testing "HEAD"
    (let [head-response
          (request
           {:request-method :head
            :uri "/pets"})
          get-response
          (request
           {:request-method :get
            :uri "/pets"})]
      (testing "HEAD is successful"
        (is (= 200 (:status head-response))))
      (testing "GET and HEAD have identical headers"
        (is (= (:headers get-response) (:headers head-response))))
      (testing "Body of HEAD is nil"
        (is (nil? (:body head-response)))
        ;; And not just because the GET body is nil
        (is (not (nil? (:body get-response))))))))

;; Tests conditional requests
(deftest not-modified-test
  (testing "Conditional requests"

    (let [{:keys [status headers body]}
          (request
           {:request-method :get
            :uri "/pets"})]

      (testing "cache-update"

        (let [last-modified-date (get headers "Last-Modified")]

          (testing "last-modified header returned as expected"
            (is (= "Tue, 4 Dec 2012 04:21:00 GMT" last-modified-date)))

          (let [{:keys [status headers body]}
                (request {:request-method :get
                          :uri "/pets"
                          :headers {"if-modified-since" last-modified-date}})]

            (testing "304 if we use the same date in request"
              (is (= 304 status))))

          (let [{:keys [status headers body]}
                (request {:request-method :get
                          :uri "/pets"
                          :headers {"if-modified-since"
                                    (to-rfc-1123-date-time
                                     (java.time.Instant/parse "2019-01-01T00:00:00Z"))}})]

            (testing "304 if we use a future date in the request"
              (is (= 304 status))))

          (let [{:keys [status headers body]}
                (request {:request-method :get
                          :uri "/pets"
                          :headers {"if-modified-since"
                                    (to-rfc-1123-date-time
                                     (java.time.Instant/parse "2010-01-01T00:00:00Z"))}})]

            (testing "200 if we use a prior date in the request"
              (is (= 200 status)))

            (testing "last-modified header returned as normal when if-modified-since request header exists"
              (is (= "Tue, 4 Dec 2012 04:21:00 GMT" (get headers "Last-Modified"))))))))))
