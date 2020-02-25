;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.alpha.oauth2.oic
  "opts is always a map containing the following:

  :openid-config - a map (read from JSON, with string keys preserved)
  of the OpenID Provider Metadata, see
  https://openid.net/specs/openid-connect-discovery-1_0.html#ProviderConfig

  :client-id - a string representing the id of the OAuth2 client

  :client-secret - a string representing the OAuth2 client secret

  :redirect-uri - a string representing an absolute URI of the OAuth2
  redirect URI

  :jwks - a map containing the JWKS key set (read from JSON, with
  string keys preserved)

  :scope - scope query parameter sent to the
  authorization_endpoint (optional, defaults to 'openid'). Overridden
  values must contain openid.

  IMPORTANT: All handlers in this namespace require ring-session to be
  present in the middleware chain.
  "
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

(defn- login-url [state opts]
  (let [{:keys [openid-config client-id redirect-uri]} opts]
    ;; TODO: Got these clues from onelogin docs but verify against
    ;; RFCs, or
    ;; https://openid.net/specs/openid-connect-discovery-1_0.html#ProviderConfig
    (let [authorization-endpoint (get openid-config "authorization_endpoint")]
      ;; TODO: Use a URL builder (see java.net.URL ?) - or Stuart
      ;; Sierra's wrapper, since authorization_endpoint may contain
      ;; query params which need to be auto-escaped

      ;; TODO: The nonce parameter is from the onelogin documentation - it
      ;; doesn't appear to be used by Cognito - check the RFCs.

      ;; TODO: Support Authorization Code Grant with PKCE as an option
      ;; https://tools.ietf.org/html/rfc7636
      ;; https://nat.sakimura.org/2016/01/25/cut-and-pasted-code-attack-in-oauth-2-0-rfc6749/
      ;; Also https://docs.aws.amazon.com/cognito/latest/developerguide/authorization-endpoint.html
      ;; This requires the addition of a code_challenge_method (S256) and code_challenge of CODE_CHALLENGE - check openid-config to see support

      (format
       "%s?client_id=%s&redirect_uri=%s&response_type=%s&scope=%s&nonce=%s&state=%s"
       authorization-endpoint
       client-id
       redirect-uri
       "code"
       (get opts :scope "openid")
       (new Nonce)
       state))))

(defn init-handler
  ([req opts]
   (let [session (:session req)
         ;; We create some state that we'll send to the OAuth2
         ;; Authentication Server and also store in our
         ;; session. That way we'll be able to defend against CSRF attacks.
         ;; See https://tools.ietf.org/html/rfc6749#section-10.12

         ;; The com.nimbusds.openid.connect.sdk.Nonce instance, from
         ;; javadoc: "Creates a new nonce with a randomly generated
         ;; 256-bit (32-byte) value, Base64URL-encoded.".  This meets
         ;; the requirement in
         ;; https://tools.ietf.org/html/rfc6749#section-10.10 for
         ;; greater than 160-bits of entropy.
         state (str (new Nonce))]
     (merge
      (response/redirect (login-url state opts))
      {:session (merge session {:apex.oauth2/state state})})))
  ([req respond raise opts]
   (respond (init-handler req opts))))

(defn create-init-handler
  "Wraps init-handler where opts is constant. Some applications may want
  to refresh opts between restarts, for example, openid-configuration
  may change periodically."
  [opts]
  (fn this
    ([req]
     (this req identity #(throw %)))
    ([req respond raise]
     (respond (init-handler req opts)))))

;; TODO: Consider adding periodic/function-based value
;; refetching/cacheing for convenience.

;; TODO: Readup on OAuth2 as to the preferred name of this

;; TODO: callback-handler has poor cohesion with its required query
;; parameters. Ideally, a classic Ring version of this handler can be
;; provided that wraps params/wrap-openapi-params. (How to signal the
;; requirement of wrap-session is another matter). Another Reitit
;; version could be provided that is traceable (by Apex's
;; trace-console).

(defn callback-handler
  "Create a Ring handler that is the target for a redirect by an OAuth2
  provider. Must take query parameters 'state' and 'code'. The
  argument on-success is a function which takes the request (plus
  respond and raise in the asynchronous form), and a map
  containing (at least) string keys for id_token and access_token."
  ([req on-success opts]
   (throw (ex-info "TODO: Implement sync version of callback-handler" {})))
  ([req respond raise on-success opts]
   (let [{:keys [redirect-uri
                 client-id
                 client-secret
                 openid-config
                 jwks]} opts
         session (:session req)

         code (get-in req [:apex/params :query "code" :value])
         state (get-in req [:apex/params :query "state" :value])
         original-state (:apex.oauth2/state session)]

     (when-not original-state
       (raise (ex-info "No session state, system cannot defend against a CSRF attack" {})))

     (when-not state
       (raise (ex-info "No state query parameter returned from provider" {})))

     ;; TODO: should we encode an expiry into the state?

     (when-not (= state original-state)
       ;; See OpenID Connect 3.1.2.7
       ;; https://openid.net/specs/openid-connect-core-1_0.html#AuthResponseValidation
       ;; RFC 6749, especially Sections 4.1.2 and 10.12.
       (raise (ex-info "State returned from provided doesn't match that in session" {})))

     ;; TODO: Can also encode the client id and client secret in the
     ;; Authorization request header with:
     ;; Authorization: Basic Base64Encode(client_id:client_secret)

     (let [token-url (get openid-config "token_endpoint")
           client (http/new-client)
           body (http/->www-form-urlencoded
                 {"grant_type" "authorization_code"
                  "code" code
                  "redirect_uri" redirect-uri
                  "client_id" client-id
                  "client_secret" client-secret})]

       ;; TODO: Support PKCE's code_verifier

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
                     id-token (get body "id_token")

                     ;; TODO: this may be ok, because may not be
                     ;; passing the 'openid' scope -so in this case
                     ;; it's not particularly an error
                     ;; _ (when-not id-token (raise (ex-info "No id-token in response" {})))

                     id-token-jwt  (when id-token (jwt/signed-jwt id-token))

                     access-token (get body "access_token")
                     _ (when-not access-token (raise (ex-info "No access-token in response" {})))
                     access-token-jwt (jwt/signed-jwt access-token)
                     ]

                 ;; TODO: Check id-token is valid (3.1.3.7) - e.g. the
                 ;; issuer identifier must match to the openid-config
                 ;; issuer.

                 (when id-token
                   (when-not
                       (jwt/validate-jws id-token-jwt jwks)
                     (raise
                      (ex-info "Claims have an invalid signature" {:apex.response/status 400}))))

                 (when-not
                     (jwt/validate-jws access-token-jwt jwks)
                     (raise
                      (ex-info "Access token has invalid signature" {:apex.response/status 400})))

                 ;; TODO: Always check expiry of tokens, here and in the middleware for each request

                 ;; TODO: How to revoke these JWTs? What part of the
                 ;; JWT corresponds to the id that can be revoked?

                 (on-success
                  req respond raise
                  (merge
                   (when id-token
                     {:apex.oic/id-token-claims (jwt/claims id-token-jwt)})
                   {:apex.oic/access-token-claims (jwt/claims access-token-jwt)
                    :apex.oic/access-token access-token})))

               (catch Exception e
                 (raise e)))

             (raise
              (let [body (slurp (.body result))]
                (ex-info
                 ;; TODO: Might get "error": "invalid_grant" if (in
                 ;; keycloak) the user doesn't consent to one or more
                 ;; of the scopes requested.
                 (format "Token exchange request returned a non-OK response (%s) - %s" (.statusCode result) body)
                 {:status (.statusCode result)
                  :body body})))))

         :on-error raise ; TODO: Possibly should augment error with context
         })))))

(defn create-callback-handler
  [opts]

  (fn this
    ([req]
     (callback-handler req opts))

    ([req respond raise]
     (callback-handler req respond raise opts))))

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
