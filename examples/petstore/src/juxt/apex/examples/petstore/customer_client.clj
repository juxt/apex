;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.examples.petstore.customer-client
  (:require
   [reitit.ring :as ring]
   [clojure.java.io :as io]
   [juxt.apex.alpha.oauth2.oic :as oic]
   [ring.middleware.session :as session]
   [juxt.apex.alpha.oauth2.jwt :as jwt]
   [jsonista.core :as jsonista]
   [ring.util.response :as response]
   [integrant.core :as ig]
   [juxt.apex.alpha.params.parameters :as params]
   [juxt.apex.alpha.html.html :as html]
   [ring.adapter.jetty :as jetty]))

(defn create-root-handler
  ([opts]

   (let [{:keys [apex/request-history-atom
                 apex/session-opts
                 client-id
                 client-secret
                 openid-config-url]} opts
         openid-config (jsonista/read-value (slurp openid-config-url))]

     (ring/ring-handler
      (ring/router
       [
        ["/" {:get (fn [req respond raise] (respond (response/redirect "/index")))}]
        ["/index"
         {:get
          (fn [req respond raise]
            (respond
             {:status 200
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
                       (str "Welcome!\n" (pr-str (get-in req [:session :subject])))
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
                     :redirect-uri "http://localhost:8091/openid/callback"
                     :jwks jwks
                     }]
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
                 ;; TODO: This callback needs to take something that is in 'user-space'.
                 ;;
                 (let [on-success
                       (fn [req respond raise {:apex.openid/keys [claims]}]
                         (respond
                          (conj
                           (response/redirect
                            "/index"
                            :see-other)
                           [:session
                            ;; Iff we can use session keys mapped to an
                            ;; internal session database, then it's ok to
                            ;; cache the claims between the requests, TODO:
                            ;; we should do exactly this.
                            {:subject {:iss (get claims "iss")
                                       :sub (get claims "sub")}}]))
                         )]
                   (oic/callback-handler req respond raise on-success opts))))

              :middleware
              [[session/wrap-session session-opts]
               [params/wrap-openapi-params
                [{"name" "state" "in" "query" "required" "true" "style" "form"}
                 {"name" "code" "in" "query" "required" "true" "style" "form"}]]]}]])]])

      ;; TODO: Create a client-app friendly not found handler
      (ring/create-default-handler
       {:not-found
        (let [not-found-response
              {:status 404
               :headers {"content-type" "text/plain"}
               :body "customer-client: Not found\n"}]
          (fn
            ([req] not-found-response)
            ([req respond raise] (respond not-found-response))))})))))

(defmethod ig/init-key ::jetty
  [_ {:keys [juxt.apex.examples/listener-port
             juxt.apex.examples.client/auth-config
             juxt.apex.examples.client/cookie-name]
      :as opts}]

  (let [request-history-atom (atom [])
        session-opts
        {:store (ring.middleware.session.memory/memory-store (atom {}))
         :cookie-name cookie-name
         }]
    (jetty/run-jetty
     (create-root-handler
      (merge
       auth-config
       {:apex/request-history-atom request-history-atom
        :apex/session-opts session-opts}))
     (-> opts
         (dissoc :handler)
         (assoc :port listener-port
                :join? false
                :async? true
                :async-timeout 5000)))))

(defmethod ig/halt-key! ::jetty [_ server]
  (.stop server))
