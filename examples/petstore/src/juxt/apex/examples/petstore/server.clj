;; Copyright © 2020, JUXT LTD.

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
               (params/wrap-openapi-params (get openapi-operation "parameters"))
               )})]]


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

      (console/trace-console opts)]

     {:reitit.middleware/transform (trace/trace-middleware-transform request-history-atom)})))

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

(defmethod ig/init-key ::jetty
  [_ {:keys [juxt.apex.dev/new-handler-on-each-request?
             juxt.apex.examples.petstore.client/port]
      :as opts}]

  (let [request-history-atom (atom [])]
    (jetty/run-jetty
     (if new-handler-on-each-request?
       (fn [req respond raise]
         (let [h (create-root-handler {:apex/request-history-atom request-history-atom})]
           (h req respond raise)))
       ;; Production
       (create-root-handler {:apex/request-history-atom request-history-atom}))
     (-> opts
         (dissoc :handler)
         (assoc :port port
                :join? false
                :async? true
                :async-timeout 5000)))))

(defmethod ig/halt-key! ::jetty [_ server]
  (.stop server))
