;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.examples.tutorial.tutorial-test
  (:require
   [clojure.test :refer [deftest is]]
   [juxt.apex.alpha.http.core :as http]
   [juxt.reap.alpha.decoders :as reap]
   [juxt.pick.alpha.core :refer [pick]]
   [juxt.pick.alpha.apache :refer [using-apache-algo]]))

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
              (conj
               response
               [:body (:juxt.http/content resource)])))
          h (http/handler provider)]
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
              (conj
               response
               [:body (:juxt.http/content resource)])))
          h (http/handler provider)]
      (:body
       (h {:scheme :https
           :uri "/"
           :request-method :get}))))))

;; Conditional requests



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
          (conj
           response
           [:body (:juxt.http/content resource)])))

      h (http/handler provider)
      first-response (h {:scheme :https
                         :uri "/"
                         :request-method :get})
      last-modified (get-in first-response [:headers "last-modified"])]

  (h {:scheme :https
      :uri "/"
      :request-method :get}))


#_(.
 (java.time.format.DateTimeFormatter/RFC_1123_DATE_TIME)
 parse
 "Sat, 4 Jul 2020 10:00:00 +0100"
 )
