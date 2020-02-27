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
   [juxt.apex.alpha.params.parameters :as params]
   [juxt.apex.alpha.html.html :as html]
   [reitit.core :as r]
   [ring.util.response :as response]
   reitit.middleware
   [reitit.ring :as ring]
   [ring.adapter.jetty :as jetty]))

(def database
  (atom {"1" {"name" "Sven" "tag" "dog"}
         "2" {"name" "Luna" "tag" "cat"}
         "3" {"name" "Arya" "tag" "cat"}
         "4" {"name" "Kaia" "tag" "cat"}
         "5" {"name" "Vega" "tag" "dog"}
         ;; Not quite sure what kind of pet Lizz is
         "6" {"name" "Lizz"}}))

(defn create-root-router
  [opts]

  (let [{:keys [apex/session-opts
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

                 (let [limit (get-in req [:apex/params :query "limit" :value])]
                   (respond
                    {:status 200
                     :body (str (vec (cond->> (vals @database) limit (take limit))) "\n")}))))]
          [
           ["/api/pets"
            ["/pets"
             (let [openapi-operation (get-in openapi ["paths" "/pets" "get"])]
               {
                :get
                (->
                 handler
                 (params/wrap-openapi-params (get openapi-operation "parameters"))
                 (session/wrap-session session-opts))})]]

           ]))

      ;; TODO: Improve the mocking such that each route in the OpenAPI
      ;; document is accounted for and presents a default page, perhaps
      ;; utilising the 'examples' and 'responses' section to form a
      ;; 'happy-path' response.

     ]

     )))

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

  (let [session-opts
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
                          {:apex/session-opts session-opts}))]
            (if respond
              (h req respond raise)
              (h req)))))
       ;; Production
       (create-root-handler
        (merge
         auth-config
         {:apex/session-opts session-opts})))
     (-> opts
         (dissoc :handler)
         (assoc :port listener-port
                :join? false
                :async? true
                :async-timeout 5000)))))

(defmethod ig/halt-key! ::jetty [_ server]
  (.stop server))
