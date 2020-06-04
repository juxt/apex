;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.http.content-negotiation-test
  (:require
   [clojure.test :refer [deftest is]]
   [ring.mock.request :refer [request]]
   [juxt.apex.alpha.http.content-negotiation
    :refer [acceptable-media-type select-best-representation]]))

;; This test is derived from the table in RFC 7231 Section 5.3.2.

(deftest acceptable-media-type-test
  (let [accepts (reap/accept "text/*;q=0.3, text/html;q=0.7, text/html;level=1, text/html;level=2;q=0.4, */*;q=0.5")]
    (is (= 1.0 (:qvalue
                (acceptable-media-type
                 accepts
                 {:apex.http/content-type "text/html;level=1"}))))
    (is (= 0.7 (:qvalue
                (acceptable-media-type
                 accepts
                 {:apex.http/content-type "text/html"}))))
    (is (= 0.3 (:qvalue
                (acceptable-media-type
                 accepts
                 {:apex.http/content-type "text/plain"}))))
    (is (= 0.5 (:qvalue
                (acceptable-media-type
                 accepts
                 {:apex.http/content-type "image/jpeg"}))))
    (is (= 0.4 (:qvalue
                (acceptable-media-type
                 accepts
                 {:apex.http/content-type "text/html;level=2"}))))
    (is (= 0.7 (:qvalue
                (acceptable-media-type
                 accepts
                 {:apex.http/content-type "text/html;level=3"}))))))

;; TODO: Reap should return lower-case to ensure comparisons work
;; case-insensitively. String's equalsIgnoreCase is not sufficient because
;; parameter maps need to be compared for quality.

(deftest select-best-representation-test
  (are [accept-header expected-content]
      (= expected-content
         (:id
          (select-best-representation
           (-> (request :get "/hello")
               (update
                :headers conj
                ["accept" accept-header]))

           [{:id :html
             :apex.http/content "<h1>Hello World!</h1>"
             :apex.http/content-type "text/html;charset=utf-8"}

            {:id :plain-text
             :apex.http/content "Hello World!"
             :apex.http/content-type "text/plain;charset=utf-8"}])))

    "text/html" :html
    "text/html;q=0.8,text/plain;q=0.7" :html
    "text/plain" :plain-text
    "text/html;q=0.8,text/plain" :plain-text))
