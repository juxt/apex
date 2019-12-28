;; Copyright © 2019, JUXT LTD.

(ns juxt.apex.examples.petstore.server
  (:require
   [clojure.java.io :as io]
   [integrant.core :as ig]
   [jsonista.core :as jsonista]
   [juxt.apex.alpha.openapi.openapi :as openapi]
   [juxt.apex.alpha.openapi.yaml :as yaml]
   [juxt.apex.alpha.redoc.redoc :as redoc]
   [juxt.apex.alpha.trace.trace-console :as console]
   [juxt.apex.alpha.trace.trace :as trace]
   [juxt.apex.alpha.params.parameters :as params]
   [reitit.core :as r]
   reitit.middleware
   [reitit.ring :as ring]
   [ring.adapter.jetty :as jetty]))

(def database
  (atom {"1" {"name" "Sven" "type" "Dog"}
         "2" {"name" "Luna" "type" "Cat"}
         "3" {"name" "Arya" "type" "Cat"}
         "4" {"name" "Kaia" "type" "Cat"}
         "5" {"name" "Vega" "type" "Dog"}}))

(let [doc
      (yaml/parse-string
       (slurp
        (io/resource "petstore-expanded.yaml")))]
  (get-in doc ["paths" "/pets" "get" "parameters"])
  )

(defn create-root-router [{:apex/keys [request-history-atom] :as opts}]
  (let [openapi (yaml/parse-string
                 (slurp
                  (io/resource "petstore-expanded.yaml")))]
    (ring/router
     [
      ;; Redoc
      ["/doc/pets-api/redoc.html"
       (redoc/new-redoc-handler "/doc/pets-api/swagger.json")]

      ;; Swagger UI
      ["/doc/pets-api/swagger-ui.html"
       (redoc/new-swagger-ui-handler "/doc/pets-api/swagger.json")]

      ;; TODO: Promote something like this to openapi module
      ["/doc/pets-api/swagger.json"
       {:get
        {:handler
         (fn [req respond raise]
           (respond
            {:status 200
             :headers {"content-type" "application/json"}
             :body (jsonista/write-value-as-string
                    (->
                     openapi
                     (assoc
                      "servers"
                      [{"url" "http://localhost:8080/docs/pets-api"}])))}))}}]

      (let [handler (fn this
                      ([req]
                       (this req identity #(throw %)))
                      ([req respond raise]
                       (let [limit (get-in req [:apex/params :query "limit" :value])]
                         #_(throw (ex-info "Forced exception" {:data 123
                                                               :type :forced}))
                         (respond
                          {:status 200
                           :body (str (vec (cond->> (vals @database) limit (take limit))) "\n")}))))]
        [
         ["/api/pets"
          ["/pets"
           (let [openapi-operation (get-in openapi ["paths" "/pets" "get"])]
             {
              :get
              ;; Option A: Traditional Ring wrapping
              (->
               handler
               ((params/make-wrap-openapi-params (get openapi-operation "parameters"))))})]]


         ["/api/pets2"
          ["/pets"
           (let [openapi-operation (get-in openapi ["paths" "/pets" "get"])]
             {
              :get
              ;; Option B: Reitit middleware, this approach is
              ;; compatible with Apex's tracing facility.
              handler

              :middleware
              [
               [params/openapi-parameters-middleware (get-in openapi ["paths" "/pets" "get" "parameters"])]
               ]})]]])

      #_(openapi/create-api-route
         "/api/pets"
         doc
         (merge
          opts
          {:name :pets-api}
          {:apex/add-implicit-head? false
           :apex/resources
           {"/pets"
            {:apex/methods
             {:get
              {:handler
               (let [response
                     (fn [req]
                       (let [limit (get-in req [:apex/params :query "limit" :value])]
                         #_(throw (ex-info "Forced exception" {:data 123
                                                               :type :forced}))
                         {:status 200
                          :body (str (vec (cond->> (vals @database) limit (take limit))) "\n")}))]
                 (fn
                   ([req] (response req))
                   ([req respond raise]
                    (respond (response req)))))}}}}

           :apex/middleware
           [params/wrap-coerce-parameters]
           }))

      (console/trace-console opts)])))

(defn create-root-handler
  ([] (create-root-handler {:apex/request-history-atom (atom [])}))
  ([opts]
   (ring/ring-handler
    (create-root-router opts)

    (ring/create-default-handler
     {:not-found
      (let [not-found-response
            {:status 404
             :headers {"content-type" "text/plain"}
             :body "Apex: Not found\n"}]
        (fn
          ([req] not-found-response)
          ([req respond raise] (respond not-found-response))))}))))

#_(assert
 (= {:status 200
     :body "Hello\n"}
    ((create-root-handler) {:uri "/hello" :request-method :get})))

#_(assert
 (=
  {:status 200
   :body "[{\"name\" \"Sven\", \"type\" \"Dog\"} {\"name\" \"Luna\", \"type\" \"Cat\"}]\n"}
  ((create-root-handler) {:uri "/pets-api/pets"
                          :query-string "limit=2"
                          :request-method :get})))

#_((create-root-handler) {:uri "/pets-api/pets"
                          :query-string "limit=2"
                          :request-method :get})

#_(ring/ring-handler
   (ring/router
    [["/{*path}"
      {:name :pets
       :handler handler
       :middleware
       [:clean-response
        [:server-header "JUXT Apex"]
        ]}]

     #_["/index.html"
        (fn [req respond _]
          (respond {:status 200 :body (io/input-stream (io/resource "public/index.html"))}))]

     #_["/assets/react/{*path}"
        (ring/create-resource-handler
         {:root "META-INF/resources/webjars/react/16.8.5"
          :parameter :path})]

     #_["/assets/react-dom/{*path}"
        (ring/create-resource-handler
         {:root "META-INF/resources/webjars/react-dom/16.8.5"
          :parameter :path})]

     #_["/assets/react-jsonschema-form/{*path}"
        (ring/create-resource-handler
         {:root "META-INF/resources/webjars/react-jsonschema-form/1.0.5"
          :parameter :path})]

     #_["/js/{*path}"
        (ring/create-resource-handler
         {:root "public"
          :parameter :path})]

     ]

    {:reitit.middleware/registry
     {:clean-response clean-response-middleware
      :server-header server-header-middleware

      }}))

(defmethod ig/init-key ::jetty
  [_ {:keys [new-handler-on-each-request?]
      :as opts}]

  (let [request-history-atom (atom [])]
    (jetty/run-jetty
     (if new-handler-on-each-request?
       (fn [req respond raise]
         (let [h (create-root-handler {:apex/request-history-atom request-history-atom})]
           (h req respond raise)))
       ;; Production
       (create-root-handler {:apex/request-history-atom request-history-atom}))
     (-> opts (dissoc :handler)
         (assoc :join? false
                :async? true
                :async-timeout 5000)))))

(defmethod ig/halt-key! ::jetty [_ server]
  (.stop server))
