;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.examples.cms.graphql.graphql
  (:require
   [clojure.java.io :as io]
   [jsonista.core :as json]
   [juxt.apex.alpha.http.core :as http]
   [juxt.apex.alpha.http.resource :as resource]
   [juxt.apex.alpha.http.server :as server]
   [juxt.apex.alpha.http.handler :refer [handler]]
   [juxt.reap.alpha.graphql :as reap-graphql]
   [juxt.reap.alpha.api :as reap]))

(defmulti graphql-invoke-method
  (fn [resource-provider
       server-provider
       resource response request respond raise]
    [(:request-method request)
     (get-in resource [:juxt.http/methods (:request-method request)])]))

(defmethod graphql-invoke-method [:head nil]
  [resource-provider
   server-provider
   resource response request respond raise]
  (respond response))

(defmethod graphql-invoke-method [:get :juxt.http/content]
  [resource-provider
   server-provider
   resource response request respond raise]
  (respond
   (conj
    response
    [:body (:juxt.http/content resource)])))

(defmethod graphql-invoke-method [:get :crux.eql/schema]
  [resource-provider
   server-provider
   resource response request respond raise]
  (respond
   (conj
    response
    [:body (pr-str (:crux.eql/schema resource))])))

(defmethod graphql-invoke-method [:post :graphql-query]
  [resource-provider
   server-provider
   resource response request respond raise]

  (server/request-body-as-stream
   server-provider
   request
   (fn [body-as-byte-stream]
     (try
       (let [{query "query"
              operation-name "operationName"
              variables "variables"
              :as body}
             (json/read-value body-as-byte-stream)]

         (respond
          (conj
           response
           [:body (pr-str (reap/decode reap-graphql/OperationDefinition query))])))
       (catch Throwable t
         (raise (ex-info "Failed to parse request body as json" {} t)))))))

(defn graphql-router [server]
  (handler
   (reify
     resource/ResourceLocator
     (locate-resource [_ uri]
       (case (str uri)
         "http://localhost:8000/"
         {:juxt.http/content (slurp (io/resource "juxt/apex/examples/cms/graphql/index.html"))
          :juxt.http/methods
          {:get :juxt.http/content
           :head nil}}

         "http://localhost:8000/graphql"
         {:juxt.http/methods
          {:get :crux.eql/schema
           :head nil
           :post :graphql-query}
          :crux.eql/schema
          '[{:app/dashboard
             [:title
              {(:user
                {:resolver
                 {:crux/query
                  {:find [?u]
                   :where [[?u :crux.db/id ?subject]
                           ;; For this entity, the resource IS the subject!
                           (authorized? ?subject ?subject ?action)]}
                  :debug false
                  }})
               [:role :name]}
              {(:sidebar
                {:resolver
                 {:crux/query
                  {:find [?sb]
                   :where [[?sb :container %]
                           (authorized? ?subject ?sb ?action)]}
                  :debug false
                  }})
               [:label
                (:description {:default "None"})
                :role]}

              {(:customer-table
                {:resolver
                 {:crux/query
                  {:find [?customer]
                   :where [[?customer :juxt/type :customer]]}
                  }})

               [:crux.db/id
                :juxt/type
                {(:health-record
                  {:resolver
                   { ;;:debug :dry-run
                    :crux/query
                    {:find [?r]
                     :where [[?r :crux.db/id _]]}}})
                 [:weight :conditions]}
                ]}]}]}

         nil))

     resource/Resource
     (invoke-method [resource-provider server-provider
                     resource response request respond raise]

       (graphql-invoke-method
        resource-provider
        server-provider
        resource response request respond raise)))

   server))
