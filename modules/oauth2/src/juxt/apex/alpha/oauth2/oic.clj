;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.alpha.oauth2.oic
  (:require
   [juxt.apex.alpha2.jwt :as jwt]
   [clojure.string :as str]
   [clojure.java.io :as io]
   [jsonista.core :as json]
   [ring.util.response :as response]
   [juxt.apex.alpha2.openapi :as openapi]
   [juxt.apex.alpha2.http-client :as http]
   [juxt.apex.yaml :as yaml])
  (:import
   (java.net.http HttpRequest$BodyPublishers HttpResponse$BodyHandlers)
   (com.nimbusds.openid.connect.sdk Nonce)))

(defn login-url [state {:keys [openid-config-f client-id redirect-uri]}]
  (let [authorization-endpoint (get (openid-config-f) "authorization_endpoint")]
    (format
     "%s?client_id=%s&redirect_uri=%s&response_type=%s&scope=%s&nonce=%s&state=%s"
     authorization-endpoint
     client-id
     redirect-uri
     "code"
     "openid"
     (new Nonce)
     state)))

(defn create-init-handler
  [opts]
  (fn
    ([req]
     (throw (ex-info "Sync not yet supported" {})))
    ([req respond raise]
     (let [session (:session req)
           state (str (new Nonce))]
       (println "create-init-handler: session is" session)
       (respond
        (merge
         (response/redirect (login-url state opts))
         {:session (merge session {:state state})}))))))

(defn create-callback-handler
  [{:keys [redirect-uri
           client-id
           client-secret
           openid-config-f
           jwks-f
           on-success]}]

  (assert openid-config-f)
  (assert jwks-f)

  (fn
    ([req]
     (throw (ex-info "Sync not yet supported" {})))

    ([req respond raise]

     (assert openid-config-f)
     (assert jwks-f)

     (let [session (:session req)
           _ (println "session is" session)
           code (get-in req [:apex/params :query "code" :value])
           state (get-in req [:apex/params :query "state" :value])]

       (when-not state
         (raise (ex-info "No state query parameter returned from provider" {})))

       (when-not (:state session)
         (raise (ex-info "No session state, system cannot defend against a CSRF attack" {})))

       (when-not (= state (:state session))
         (raise (ex-info "State returned from provided doesn't match that in session" {})))

       (let [openid-config (openid-config-f)
             token-url (get openid-config "token_endpoint")
             client (http/new-client)
             body (http/->www-form-urlencoded
                   {"grant_type" "authorization_code"
                    "code" code
                    "redirect_uri" redirect-uri
                    "client_id" client-id
                    "client_secret" client-secret})]

         (http/request
          client :post

          token-url

          {:headers
           {"Content-Type" "application/x-www-form-urlencoded"}
           :request-body-publisher
           ;; TODO: Could use a protocol
           (HttpRequest$BodyPublishers/ofString body)

           :response-body-handler
           (HttpResponse$BodyHandlers/ofInputStream)

           :async true
           :on-success
           (fn [result]
             ;; TODO: Check we get a 200!
             ;; TODO: We'll throw an error that should be picked up by the trace console

             ;; TODO: Make the errors as good as they can be
             ;; TODO: Verify id-token, decode id-token and set cookie on response
             (try
               (let [body (json/read-value (.body result))
                     id-token (get body "id_token")
                     _ (when-not id-token (raise (ex-info "No id-token in response" {})))
                     id-token-jwt (jwt/signed-jwt id-token)
                     jwks (jwks-f)]

                 (if (jwt/validate-jws id-token-jwt jwks)
                   (on-success
                    (assoc req
                           ;; TODO: document me!
                           :apex.oic/claims (jwt/claims id-token-jwt))
                    respond
                    raise)
                   (raise (ex-info "Claims have an invalid signature" {:apex.response/status 400}))))

               (catch Exception e
                 (raise e))))

           :on-error raise ; TODO: Possibly should augment error with context
           }))))))

(defn api [path
           {:keys
            [redirect-uri
             client-id
             client-secret
             openid-config-f ; a function to retrieve the openid-config - which could change
             jwks-f ; a function that returns a com.nimbusds.jose.jwk.JWKSet, can use jwt/jwks
             on-success ; (fn [req] [req respond raise]) ; claims are assoc'd to request :apex.oic/claims
             ]
            :as opts}]
  (assert (:apex/request-history-atom opts))
  (assert redirect-uri)
  (assert client-id)
  (assert client-secret)
  (assert openid-config-f)
  (assert jwks-f)
  (assert on-success)
  (openapi/create-api-route
   path
   (yaml/parse-string
    (slurp
     (io/resource "juxt/apex/alpha2/oic.yaml")))
   (merge
    opts
    {:apex/add-implicit-head? false
     :apex/resources
     {"/init"
      {:apex/methods
       {:get
        {:handler (create-init-handler opts)
         }}}
      "/callback"
      {:apex/methods
       {:get
        {:handler (create-callback-handler opts)
         }}}}})))
