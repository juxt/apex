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
         {:juxt.http/content (slurp (io/resource "juxt/apex/examples/cms/graphql/index.html"))
          :juxt.http/methods #{:get :head}}
         "http://localhost:8000/graphql"
         {:juxt.http/content "Graphql\n"
          :juxt.http/methods #{:get :head :post}}
         :else nil))

     http/Resource
     (invoke-method [_ resource response request respond raise]
       (case (:request-method request)
         :head (respond {:status 200})
         :get (respond {:status 200
                        :body (:juxt.http/content resource)})
         ))

     http/ServerOptions
     (server-header [_] "Apex"))))
