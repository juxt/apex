;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.examples.petstore.api-server
  (:require
   ring.middleware.session.cookie
   [ring.middleware.session :as session]
   [clojure.java.io :as io]
   [juxt.apex.alpha.oauth2.oic :as oic]
   [integrant.core :as ig]
   [jsonista.core :as jsonista]
   [juxt.apex.alpha.openapi.openapi :as openapi]
   [juxt.apex.alpha.openapi.yaml :as yaml]
   [juxt.apex.alpha.oauth2.jwt :as jwt]
   [juxt.apex.alpha.redoc.redoc :as redoc]
   [juxt.apex.alpha.trace.trace-console :as console]
   [juxt.apex.alpha.trace.trace :as trace]
   [juxt.apex.alpha.params.parameters :as params]
   [juxt.apex.alpha.html.html :as html]
   [reitit.core :as r]
   [ring.util.response :as response]
   reitit.middleware
   [reitit.ring :as ring]
   [ring.adapter.jetty :as jetty]))

(def database
  (atom {"1" {"name" "Sven" "type" "Dog"}
         "2" {"name" "Luna" "type" "Cat"}
         "3" {"name" "Arya" "type" "Cat"}
         "4" {"name" "Kaia" "type" "Cat"}
         "5" {"name" "Vega" "type" "Dog"}}))

(defn create-root-router
  [opts]

  (let [{:keys [apex/request-history-atom
                apex/session-opts
                client-id
                client-secret
                openid-config-url]} opts

        _ (println "openid-config-url is" openid-config-url)
        openid-config (jsonista/read-value (slurp openid-config-url))]

    (ring/router
     [
      (let [openapi (yaml/parse-string
                     (slurp
                      (io/resource "petstore-expanded.yaml")))]
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
                        [{"url" "http://localhost:8090/docs/pets-api"}])))}))}}]

        (let [handler
              (fn this
                ([req]
                 (this req identity #(throw %)))
                ([req respond raise]

                 ;; OK, let's have a look at the session here
                 (println "sesssion keys are" (keys (:session req)))

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
                 (session/wrap-session session-opts))})]]

           ["/api/pets2"
            ["/pets"
             (let [openapi-operation (get-in openapi ["paths" "/pets" "get"])]
               {
                :get
                ;; Option B: Reitit middleware, this approach is
                ;; compatible with Apex's tracing facility.
                handler

                ;; TODO: It might be that Apex is 'a bespoke set of Ring
                ;; middleware, each of which is configurable via
                ;; OpenAPI'. Put this statement in the documentation.
                ;;
                ;; TODO?: Perhaps return the collection of middleware from a custom
                ;; 'builder', each middleware of the result would be
                ;; separately compiled its :compile step.

                :middleware
                [
                 [params/openapi-parameters-middleware
                  (get-in openapi ["paths" "/pets" "get" "parameters"])]
                 [session/wrap-session session-opts]]})]]]))

      ;; TODO: Improve the mocking such that each route in the OpenAPI
      ;; document is accounted for and presents a default page, perhaps
      ;; utilising the 'examples' and 'responses' section to form a
      ;; 'happy-path' response.

      (console/trace-console opts)]

     {:reitit.middleware/transform (trace/trace-middleware-transform request-history-atom)})))

(defn create-root-handler
  ([opts]
   (ring/ring-handler
    (create-root-router opts)

    (ring/create-default-handler
     {:not-found
      (let [not-found-response
            {:status 404
             :headers {"content-type" "text/plain"}
             :body "api-server: Not found\n"}]
        (fn
          ([req] not-found-response)
          ([req respond raise] (respond not-found-response))))}))))

(defmethod ig/init-key ::jetty
  [_ {:keys [juxt.apex.dev/new-handler-on-each-request?
             juxt.apex.examples/listener-port
             juxt.apex.examples.client/auth-config
             juxt.apex.examples.client/cookie-name]
      :as opts}]

  (let [request-history-atom (atom [])
        session-opts
        {:store (ring.middleware.session.memory/memory-store (atom {}))
         :cookie-name cookie-name}]

    (jetty/run-jetty
     (if new-handler-on-each-request?
       (fn this
         ([req]
          (this req nil nil))
         ([req respond raise]
          (let [h (create-root-handler
                   (merge auth-config
                          {:apex/request-history-atom request-history-atom
                           :apex/session-opts session-opts}))]
            (if respond
              (h req respond raise)
              (h req)))))
       ;; Production
       (create-root-handler
        (merge
         auth-config
         {:apex/request-history-atom request-history-atom
          :apex/session-opts session-opts})))
     (-> opts
         (dissoc :handler)
         (assoc :port listener-port
                :join? false
                :async? true
                :async-timeout 5000)))))

(defmethod ig/halt-key! ::jetty [_ server]
  (.stop server))
