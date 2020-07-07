;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.examples.tutorial.tutorial-test
  (:require
   [clojure.test :refer [deftest is]]
   [juxt.apex.alpha.http.handler :as handler]
   [juxt.apex.alpha.http.core :as http]
   [juxt.reap.alpha.decoders :as reap]
   [juxt.pick.alpha.core :refer [pick]]
   [juxt.pick.alpha.apache :refer [using-apache-algo]]))

;; TODO: Make a better response equality check better for testing, possibly
;; exploiting clojure.test/assert-predicate
(defn wrap-remove-header [h header]
  (fn [req]
    (->
     (h req)
     (update :headers dissoc header))))

#_(deftest locate-resource-test
  (let [h (-> (http/handler
               (reify
                 http/ResourceLocator
                 (locate-resource [_ uri]
                   (when (= (.getPath uri) "/hello.txt")
                     {:apex.http/content "Hello World!"}))
                 http/ResponseBody
                 (send-ok-response
                     [_ resource response request respond raise]
                     (respond
                      (conj response [:body (:apex.http/content resource)])))))
              (wrap-remove-header "date"))]
    (is (=
         {:status 200
          :headers {}
          :body "Hello World!"}
         (h (request :get "/hello.txt"))))

    (is (=
         {:status 404
          :headers {}}
         (h (request :get "/not-exists"))))))

(deftest response-body
  (is
   (=
    "Hello World!"
    (let [provider
          (reify
            http/ResourceLocator
            (locate-resource [this uri] {:juxt.http/content "Hello World!"})
            http/ResponseBody
            (send-ok-response [this resource response request respond raise]
              (respond
               (conj
                response
                [:body (:juxt.http/content resource)]))))
          h (handler/handler provider)]
      (:body
       (h {:scheme :https
           :uri "/"
           :request-method :get}))))))

;; Content negotiation

(deftest content-negotiation
  (is
   (=
    "Hello World!"
    (let [provider
          (reify
            http/ResourceLocator
            (locate-resource [this uri] {})

            http/ContentNegotiation
            (best-representation [_ resource request]
              {:juxt.http/variants
               [{:juxt.http/content "Hello World!"
                 :juxt.http/content-language (reap/content-type "text/plain")}]})

            http/ResponseBody
            (send-ok-response [this resource response request respond raise]
              (respond
               (conj
                response
                [:body (:juxt.http/content resource)]))))
          h (handler/handler provider)]
      (:body
       (h {:scheme :https
           :uri "/"
           :request-method :get}))))))

;; Conditional requests


(deftest conditional-request-with-last-modified
  (let [provider
        (reify
          http/ResourceLocator
          (locate-resource [this uri]
            {:juxt.http/content "Hello World!"
             :juxt.http/last-modified (java.time.ZonedDateTime/parse "2020-07-04T10:00:00.000+01:00[Europe/London]")})

          http/LastModified
          (last-modified [_ representation]
            (:juxt.http/last-modified representation))

          http/ResponseBody
          (send-ok-response [this resource response request respond raise]
            (respond
             (conj
              response
              [:body (:juxt.http/content resource)]))))

        h (handler/handler provider)
        first-response (h {:scheme :https
                           :uri "/"
                           :request-method :get})

        last-modified (get-in first-response [:headers "last-modified"])

        second-response (h {:scheme :https
                            :uri "/"
                            :request-method :get
                            :headers {"if-modified-since" last-modified}})]

    (is (= 200 (:status first-response)))
    (is (= 304 (:status second-response)))))

#_(deftest content-negotiation-test
  (let [h (->
           (http/handler
            (reify
              http/ResourceLocator
              (locate-resource [_ uri]
                (case (.getPath uri)
                  "/hello"
                  {:apex.http/variants
                   [(java.net.URI. "/hello.html")
                    (java.net.URI. "/hello.txt")]}
                  "/hello.html"
                  {:apex.http/content "<h1>Hello World!</h1>"
                   :apex.http/content-type "text/html;charset=utf-8"}
                  "/hello.txt"
                  {:apex.http/content "Hello World!"
                   :apex.http/content-type "text/plain;charset=utf-8"}
                  ;; else not found
                  nil))

              http/ContentNegotiation
              (best-representation [provider resource request]
                (:juxt.http/variant
                 (conneg/select-variant
                  {:juxt.http/request
                   request
                   :juxt.http/variants
                   (map #(http/lookup-resource provider %) (:apex.http/variants resource))})))

              http/ResponseBody
              (send-ok-response
                  [_ resource response request respond raise]
                  (respond
                   (conj response [:body (:apex.http/content resource)])))))
           ;; Help with fixed comparisons
           (wrap-remove-header "date"))]
    (is (=
         {:status 200
          :headers {"content-location" "/hello.html"}
          :body "<h1>Hello World!</h1>"}
         (h (update
             (request :get "/hello")
             :headers conj ["accept" "text/html"]))))))
