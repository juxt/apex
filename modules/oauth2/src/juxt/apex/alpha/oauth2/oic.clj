;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.alpha.oauth2.oic
  (:require
   [juxt.apex.alpha.oauth2.jwt :as jwt]
   [juxt.apex.alpha.oauth2.http-client :as http]
   [clojure.string :as str]
   [clojure.java.io :as io]
   [clojure.pprint :refer [pprint]]
   [jsonista.core :as json]
   [ring.util.response :as response]
   [juxt.apex.alpha.openapi.openapi :as openapi]
   [juxt.apex.alpha.openapi.yaml :as yaml])
  (:import
   (java.net.http HttpRequest$BodyPublishers HttpResponse$BodyHandlers)
   (com.nimbusds.openid.connect.sdk Nonce)))

(defn login-url [state {:keys [openid-config-f client-id redirect-uri]}]
  (assert (ifn? openid-config-f))
  (assert client-id)
  (assert redirect-uri)
  ;; TODO: Got these clues from onelogin docs but verify against RFCs
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
  (fn this
    ([req]
     (this req identity #(throw %)))
    ([req respond raise]
     (let [session (:session req)
           ;; We create some state that we'll send to the OAuth2
           ;; Authentication Server and also store in our
           ;; session. That way we'll be able to defend against fake
           ;; callbacks. TODO: link to further documentation on this
           ;; security defence.
           state (str (new Nonce))]
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
           success-uri]}]

  (assert openid-config-f)
  (assert jwks-f)

  (fn this
    ([req]
     (this req identity #(throw %)))

    ([req respond raise]

     (assert openid-config-f)
     (assert jwks-f)
     (assert success-uri)

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
           (fn [^java.net.http.HttpResponse result]

             ;; TODO: Check we get a 200!
             ;; TODO: We'll throw an error that should be picked up by the trace console

             ;; TODO: Make the errors as good as they can be
             ;; TODO: Verify id-token, decode id-token and set cookie on response
             (case (.statusCode result)
               200
               (try
                 (let [body (json/read-value (.body result))
                       _ (println "body:" body)
                       id-token (get body "id_token")
                       _ (when-not id-token (raise (ex-info "No id-token in response" {})))
                       id-token-jwt (jwt/signed-jwt id-token)

                       access-token (get body "access_token")
                       _ (when-not access-token (raise (ex-info "No access-token in response" {})))
                       access-token-jwt (jwt/signed-jwt access-token)

                       jwks (jwks-f)]

                   (when-not
                       (jwt/validate-jws id-token-jwt jwks)
                       (raise
                        (ex-info "Claims have an invalid signature" {:apex.response/status 400})))

                   (when-not
                       (jwt/validate-jws access-token-jwt jwks)
                       (raise
                        (ex-info "Access token has invalid signature" {:apex.response/status 400})))

                   _ (println "jwt/claims of access token:")
                   _ (pprint (jwt/claims access-token-jwt))

                   ;; TODO: Always check expiry of tokens, here and in the middleware for each request

                   ;; TODO: How to revoke these JWTs? What part of the
                   ;; JWT corresponds to the id that can be revoked?

                   (respond
                    (conj
                     (response/redirect
                      success-uri
                      :see-other)
                     [:session
                      ;; Iff we can use session keys mapped to an
                      ;; internal session database, then it's ok to
                      ;; cache the claims between the requests, TODO:
                      ;; we should do exactly this.
                      {"id_token" id-token
                       "access_token" access-token}]))


                   #_(on-success
                      ;; TODO: Should redirect here, because otherwise
                      ;; we're left in a state where a refresh can cause
                      ;; the error: "No session state, system cannot
                      ;; defend against a CSRF attack"
                      (assoc req
                             ;; TODO: document me!
                             ;;:apex.jwt/claims #_(jwt/claims id-token-jwt)
                             )
                      respond
                      raise))

                 (catch Exception e
                   (raise e)))

               (raise
                (ex-info
                 (format "Token exchange request returned a non-OK response (%s)" (.statusCode result))
                 {:status (.statusCode result)}))))

           :on-error raise ; TODO: Possibly should augment error with context
           }))))))

(defn wrap-openid-authorization
  [handler]
  (fn this
    ([req] (this req identity #(throw %)))
    ([req respond raise]
     ;; TODO: Extract JWTs: "id_token" & "access_token" from session
     ;; TODO: Scopes must be extracted each time, they must be in the
     ;; signed JWT, they cannot form part of the cookie else they
     ;; might be forged.
     ;; TODO: Check scopes against security of operation
     (handler req respond raise))))

#_(defn api [path
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
