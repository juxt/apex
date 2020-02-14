;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.examples.petstore.server
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
   reitit.middleware

   [reitit.ring :as ring]
   [ring.adapter.jetty :as jetty])
  (:import
   (com.nimbusds.openid.connect.sdk Nonce)))

(def database
  (atom {"1" {"name" "Sven" "type" "Dog"}
         "2" {"name" "Luna" "type" "Cat"}
         "3" {"name" "Arya" "type" "Cat"}
         "4" {"name" "Kaia" "type" "Cat"}
         "5" {"name" "Vega" "type" "Dog"}}))

(defn create-root-router
  [{openid-config :apex.openid/config
    client-id :apex.openid/client-id
    client-secret :apex.openid/client-secret
    :keys [apex/request-history-atom
           apex/session-opts
           ]
    :as opts}]
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

    (let [openapi
          (yaml/parse-string
           (slurp
            (io/resource "petstore-expanded-plus-security.yaml")))]
      [
       ["/welcome"
        {:get
         (fn [req respond raise]
           (respond {:status 200
                     :headers {"content-type" "text/html;charset=utf8"}
                     :body (html/content-from-template
                            (slurp
                             (io/resource "juxt/apex/examples/petstore/welcome.html"))
                            (merge
                             (html/template-model-base)
                             {"title" "Apex Petstore"
                              "navbar"
                              (html/navbar
                               [{:title "Login"
                                 :href "/openid/login"}])
                              "body"
                              "Welcome!"
                              #_(apply str (map :content sections))}))}))

         :middleware
         [[session/wrap-session session-opts]
          [oic/wrap-openid-authorization]]}]

       ["/openid"
        (let [jwks
              (jwt/jwks
               (java.net.URL. (get openid-config "jwks_uri")))

              opts {:openid-config openid-config
                    :client-id client-id
                    :client-secret client-secret
                    :redirect-uri "http://localhost:8090/openid/callback"
                    :jwks jwks
                    :success-uri "/welcome"}]
          [
           ;; TODO: In developer mode, /login could present a page
           ;; where one of a set of built-in users can be chosen and a
           ;; id-token and access-token created that embeds the chosen
           ;; user's details along with any scopes. These users might
           ;; be configured in another JSON configuration document.

           ["/login"
            {:get
             (fn this
               ([req]
                (oic/init-handler req opts))
               ([req respond raise]
                (respond (oic/init-handler req respond raise opts))))

             :middleware
             [[session/wrap-session session-opts]]}]

           ["/callback"
            {:get
             (fn this
               ([req]
                (oic/callback-handler req opts))
               ([req respond raise]
                (oic/callback-handler req respond raise opts)))

             :middleware
             [[session/wrap-session session-opts]
              [params/wrap-openapi-params
               [{"name" "state" "in" "query" "required" "true" "style" "form"}
                {"name" "code" "in" "query" "required" "true" "style" "form"}]]]}]])]

       ;; TODO: Restore this because it's useful to be able to
       ;; demonstrate different identity providers (IdPs)
       ;; Onelogin
       #_["/onelogin"
          (let [openid-config
                ;; A local copy of https://juxt-dev.onelogin.com/oidc/.well-known/openid-configuration
                (jsonista/read-value
                 (slurp
                  (io/resource "onelogin-openid-configuration.json")))

                jwks
                ;; A local copy of https://juxt-dev.onelogin.com/oidc/certs
                (get (jsonista/read-value
                      (slurp
                       (io/resource "onelogin-jwks.json"))) "keys")

                ;; Required OAuth2 application parameters
                client-id "926c11b0-13ce-0138-8f1d-0a2a13b62018140139"
                redirect-uri "http://localhost:8090/onelogin/callback"]
            [
             ;; Onelogin example
             ;; Try alice/ilovecats
             ["/login"
              ;; need to add ring-session middleware - and explain why
              {:get
               (oic/create-init-handler
                {:client-id client-id
                 :redirect-uri redirect-uri
                 :openid-config-f (constantly openid-config)})
               :middleware
               [[session/wrap-session session-opts]
                ]}]
             ;; TODO: check for errors
             ;; e.g. http://localhost:8090/onelogin/callback?error=access_denied&error_description=End-user%20does%20not%20have%20access%20to%20this%20application&state=wBh0lN4SGHNyBOCLLR48Kwyv3sVxl0TiBP9zI6EebFM
             ["/callback"
              {:get
               (oic/create-callback-handler
                {:client-id client-id
                 :client-secret "ac41d73df09841ddeda710ac7bf6861d8b7ab371536a435f45e3e0b61291c810"
                 :redirect-uri redirect-uri
                 :openid-config-f (constantly openid-config)
                 :jwks-f (constantly jwks)
                 :on-success (fn [req respond raise]
                               (respond {:status 200 :body (str "login success!!!!!" (pr-str (:apex.oic/claims req)))}))})
               :middleware
               [[session/wrap-session session-opts]
                [params/wrap-openapi-params
                 [{"name" "state" "in" "query" "required" "true" "style" "form"}
                  {"name" "code" "in" "query" "required" "true" "style" "form"}]]]}]])]])

    (console/trace-console opts)]

   {:reitit.middleware/transform (trace/trace-middleware-transform request-history-atom)}))

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
             juxt.apex.examples.petstore.server/listener-port]
      :as opts}]

  (let [request-history-atom (atom [])
        session-opts {:store
                                        ; TODO: Consider adding :key here for sessions
                                        ; to survive resets
                      (ring.middleware.session.cookie/cookie-store)
                      :cookie-name "apex-session"
                      }]
    (jetty/run-jetty
     (if new-handler-on-each-request?
       (fn this
         ([req]
          (this req nil nil))
         ([req respond raise]
          (let [h (create-root-handler {:apex/request-history-atom request-history-atom
                                        :apex/session-opts session-opts})]
            (if respond
              (h req respond raise)
              (h req)))))
       ;; Production
       (create-root-handler {:apex/request-history-atom request-history-atom
                             :apex/session-opts session-opts
                             :apex.openid/config (jsonista/read-value
                                                  ;; TODO: Careful, this is read on each request in dev
                                                  ;; Keycloak
                                                  #_(slurp "http://localhost:8080/auth/realms/master/.well-known/openid-configuration")
                                                  ;; AWS Cognito
                                                  (slurp "https://cognito-idp.eu-west-2.amazonaws.com/eu-west-2_XtCrJJi5g/.well-known/openid-configuration"))

                             :apex.openid/client-id
                             ;; Keycloak
                             ;; "petstore"
                             ;; AWS Cognito
                             "18qbjn64mkqta3oj8370i3er47"

                             :apex.openid/client-secret
                             ;; Keycloak
                             ;; "ee185f56-197b-44c0-88e8-581781440c9b"
                             ;; AWS Cognito
                             "otturr5ogvajptdofsf8shhnvcnt82jge3lan5ja2d8l3temg5v"

                             }))
     (-> opts
         (dissoc :handler)
         (assoc :port listener-port
                :join? false
                :async? true
                :async-timeout 5000)))))

(defmethod ig/halt-key! ::jetty [_ server]
  (.stop server))
