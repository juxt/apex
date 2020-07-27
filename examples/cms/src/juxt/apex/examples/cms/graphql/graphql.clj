;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.examples.cms.graphql.graphql
  (:require
   [clojure.java.io :as io]
   [jsonista.core :as json]
   edn-query-language.core
   [juxt.apex.alpha.http.core :as http]
   [juxt.apex.examples.cms.graphql.eql :as eql]
   [juxt.apex.alpha.http.resource :as resource]
   [juxt.apex.alpha.http.server :as server]
   [juxt.apex.alpha.http.handler :refer [handler]]
   [juxt.reap.alpha.graphql :as reap-graphql]
   [juxt.reap.alpha.api :as reap]
   [crux.api :as crux]))

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

  (println "Graphql query!")

  (server/request-body-as-stream
   server-provider
   request
   (fn [body-as-byte-stream]
     (try
       (let [db (:crux/db resource)
             {query "query"
              operation-name "operationName"
              variables "variables"
              :as body}
             (json/read-value body-as-byte-stream)

             document (reap/decode reap-graphql/Document query)
             schema (:crux.eql/schema resource)
             ]

         (assert db)
         (assert body)
         (prn "body is" body)
         (assert document)
         (assert schema)

         (def document document)
         (def schema schema)
         (def update-time (java.time.ZonedDateTime/now))

         ;; TODO: Apply document to :crux.eql/schema resource
         (if (and
              (= (count document) 1)
              (= (get-in document [0 :name]) "IntrospectionQuery"))
           (respond
            (-> response
                (assoc :status 200)
                (conj
                 [:body
                  (json/write-value-as-string
                   {"data"
                    {"__schema"
                     {"queryType"
                      {"name" "Root"}
                      "mutationType" nil
                      "subscriptionType" nil
                      "types"
                      [{"kind" "OBJECT"
                        "name" "Root"
                        "description" nil
                        "fields"
                        (concat
                         (for [property schema]
                           (if (and (map? property) (= (count property) 1))
                             ;; It's a join
                             (let [[k v]
                                   ;; We call first to get the single entry out of a single-entry map
                                   (first property)]
                               (cond
                                 (keyword? k)
                                 (throw (ex-info "TODO" {}))
                                 (list? k)
                                 (let [[_ params] k
                                       {:keys [graphql/name graph/description graphql/type]} params]
                                   {"name" name
                                    "description" description
                                    "args" []
                                    "type" {"kind" "OBJECT"
                                            "name" type
                                            "ofType" nil}
                                    }
                                   ))

                               )
                             :todo
                             )
                           )

                         )
                        "inputFields" nil
                        "interfaces" []
                        "enumValues" nil
                        "possibleTypes" nil
                        }

                       {"kind" "SCALAR"
                        "name" "String"
                        "description" "blah"
                        "fields" nil
                        "inputFields" nil
                        "interfaces" nil
                        "enumValues" nil
                        "possibleTypes" nil}


                       {"kind" "OBJECT"
                        "name" "Person"
                        "description" "A person"
                        "fields" [{"name" "person_name"
                                   "description" "A person's name"
                                   "args" []
                                   "type" {"kind" "SCALAR" "name" "String" "ofType" nil}
                                   "isDeprecated" false
                                   "deprecationReason" nil}

                                  {"name" "person_email"
                                   "description" "A person's email address"
                                   "args" []
                                   "type" {"kind" "SCALAR" "name" "String" "ofType" nil}
                                   "isDeprecated" false
                                   "deprecationReason" nil}]
                        "inputFields" nil
                        "interfaces" []
                        "enumValues" nil
                        "possibleTypes" nil
                        }

                       ]
                      "directives"
                      []}}})])
                (assoc-in [:headers "content-type"] "application/json")))

           (let [query (first document)
                 ;; TODO: Replace eql/eql-query with the parsed incoming query
                 ast (edn-query-language.core/query->ast eql/eql-query)
                 #_results #_(let [db (crux/db node)
                                   ast (eql/query->ast eql-query)]
                               (query db nil ast {}))]
             (respond
              (-> response
                  (merge {:status 200
                          :body (json/write-value-as-string
                                 {"data"
                                  {"allPeople" (get (first (eql/query db nil ast {})) :all-people)}
                                  }
                                 )})
                  )))
           )

         )
       (catch Throwable t
         (println "ERROR" t)
         (raise (ex-info "Failed to parse request body as json" {} t)))))))

(comment
  (reap/decode reap-graphql/Document
               "query IntrospectionQuery {
      __schema {

        queryType { name }
        mutationType { name }
        subscriptionType { name }
        types {
          ...FullType
        }
        directives {
          name
          description

          locations
          args {
            ...InputValue
          }
        }
      }
    }

    fragment FullType on __Type {
      kind
      name
      description
      fields(includeDeprecated: true) {
        name
        description
        args {
          ...InputValue
        }
        type {
          ...TypeRef
        }
        isDeprecated
        deprecationReason
      }
      inputFields {
        ...InputValue
      }
      interfaces {
        ...TypeRef
      }
      enumValues(includeDeprecated: true) {
        name
        description
        isDeprecated
        deprecationReason
      }
      possibleTypes {
        ...TypeRef
      }
    }

    fragment InputValue on __InputValue {
      name
      description
      type { ...TypeRef }
      defaultValue
    }

    fragment TypeRef on __Type {
      kind
      name
      ofType {
        kind
        name
        ofType {
          kind
          name
          ofType {
            kind
            name
            ofType {
              kind
              name
              ofType {
                kind
                name
                ofType {
                  kind
                  name
                  ofType {
                    kind
                    name
                  }
                }
              }
            }
          }
        }
      }
    }
  "))

(defn graphql-router [server crux-node]
  (assert server)
  (handler
   (reify
     resource/ResourceLocator
     (locate-resource [_ uri]
       (case (str uri)
         "http://localhost:8000/"
         {:juxt.http/content
          (slurp
           (io/resource "juxt/apex/examples/cms/graphql/index.html"))
          :juxt.http/methods
          {:get :juxt.http/content
           :head nil}}

         "http://localhost:8000/graphql"
         (let [db (crux/db crux-node)]
           {:juxt.http/methods
            {:get :crux.eql/schema
             :head nil
             :post :graphql-query}
            :crux.eql/schema eql/eql-query
            :crux/db db
            })

         ;; 404!
         nil))

     resource/Resource
     (invoke-method [resource-provider server-provider
                     resource response request respond raise]

       (graphql-invoke-method
        resource-provider
        server-provider
        resource response request respond raise))


     resource/ResourceOptions
     (resource-options-headers [_ resource] {}))

   server))


(comment
 (let [

       query (first document)


       ]

   schema

   ))

(identity schema)
(identity update-time)

(let [schema '[{(:all-people
                 {:resolver
                  {:crux/query
                   {:find [?p]
                    :where [[?p :person/name ?name]]}
                   :debug false}
                  :graphql/name "allPeople"
                  :graphql/description "Get all the people"
                  :graphql/type "People"
                  })
                [(:person/name {:description "A person's name"
                                :graphql/name "person_name"})
                 (:person/email {:description "A person's email"
                                 :graphql/name "person_email"})]}]
      incoming-query {:operation-type "query",
                      :name "IntrospectionQuery",
                      :directives (),
                      :selection-set
                      [[:field
                        {:name "__schema",
                         :arguments (),
                         :selection-set
                         [[:field
                           {:name "queryType",
                            :arguments (),
                            :selection-set [[:field {:name "name", :arguments ()}]]}]
                          [:field
                           {:name "mutationType",
                            :arguments (),
                            :selection-set [[:field {:name "name", :arguments ()}]]}]
                          [:field
                           {:name "subscriptionType",
                            :arguments (),
                            :selection-set [[:field {:name "name", :arguments ()}]]}]
                          [:field {:name "types", :arguments (), :selection-set []}]
                          [:field
                           {:name "directives",
                            :arguments (),
                            :selection-set
                            [[:field {:name "name", :arguments ()}]
                             [:field {:name "description", :arguments ()}]
                             [:field {:name "locations", :arguments ()}]
                             [:field {:name "args", :arguments (), :selection-set []}]]}]]}]]}
      ]



  {"__schema"
   {"queryType"
    {"name" "Root"}
    "mutationType" nil
    "subscriptionType" nil
    "types"
    [{"kind" "OBJECT"
      "name" "Root"
      "description" nil
      "fields"
      (for [property schema]
        (if (and (map? property) (= (count property) 1))
          ;; It's a join
          (let [[k v]
                ;; We call first to get the single entry out of a single-entry map
                (first property)]
            (cond
              (keyword? k)
              (throw (ex-info "TODO" {}))
              (list? k)
              (let [[_ params] k
                    {:keys [graphql/name graph/description graphql/type]} params]
                {"name" name
                 "description" description
                 "args" []
                 "type" {"kind" "OBJECT"
                         "name" type
                         "ofType" nil}
                 }
                ))

            )
          :todo
          )
        )
      "inputFields" nil
      "interfaces" []
      "enumValues" nil
      "possibleTypes" nil
      }
     {"kind" "SCALAR"
      "name" "String"
      "description" "blah"
      "fields" nil
      "inputFields" nil
      "interfaces" nil
      "enumValues" nil
      "possibleTypes" nil}]
    "directives"
    []}})
