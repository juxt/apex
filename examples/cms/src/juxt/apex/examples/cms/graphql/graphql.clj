;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.examples.cms.graphql.graphql
  (:require
   [clojure.java.io :as io]
   [juxt.apex.alpha.http.core :as http]
   [juxt.apex.alpha.http.handler :refer [handler]]))


(defn graphql-router []
  (handler
   (reify
     http/ResourceLocator
     (locate-resource [_ uri]
       (case (str uri)
         "http://localhost:8000/"
         {:juxt.http/content (slurp (io/resource "juxt/apex/examples/cms/graphql/index.html"))}
         "http://localhost:8000/graphql"
         {:juxt.http/content "Graphql\n"}
         :else nil
         ))

     http/OkResponse
     (send-ok-response [_ resource response request respond raise]
       (respond {:status 200
                 :body (:juxt.http/content resource)
                 }))

     http/PostMethod
     (POST [_ resource request respond raise]
       (respond
        {:status 200
         :body "post foo"}))

     http/ServerOptions
     (server-header [_] "Apex")
     )))
