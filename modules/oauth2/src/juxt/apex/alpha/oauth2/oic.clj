;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.alpha.oauth2.oic
  "opts is always a map containing the following:

  :openid-config - a map (read from JSON, with string keys preserved)
  of the OpenID Provider Metadata, see
  https://openid.net/specs/openid-connect-discovery-1_0.html#ProviderConfig

  :client-id - a string representing the id of the OAuth2 client

  :client-secret - a string representing the OAuth2 client secret

  :redirect-uri - a string representing an absolute URI of the OAuth2 redirect URI

  :jwks - a map containing the JWKS key set (read from JSON, with string keys preserved)

  :scope - scope query parameter sent to the authorization_endpoint (optional, defaults to 'openid')

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
         ;; session. That way we'll be able to defend against fake
         ;; callbacks. TODO: link to further documentation on this
         ;; security defence.
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
(defn callback-handler
  ([req opts]
   (throw (ex-info "TODO: Implement sync version of callback-handler" {})))
  ([req respond raise opts]
   (let [{:keys [redirect-uri
                 client-id
                 client-secret
                 openid-config
                 jwks
                 success-uri]} opts
         session (:session req)

         code (get-in req [:apex/params :query "code" :value])
         state (get-in req [:apex/params :query "state" :value])
         original-state (:apex.oauth2/state session)]

     (when-not original-state
       (raise (ex-info "No session state, system cannot defend against a CSRF attack" {})))

     (when-not state
       (raise (ex-info "No state query parameter returned from provider" {})))

     (when-not (= state original-state)
       (raise (ex-info "State returned from provided doesn't match that in session" {})))

     (let [token-url (get openid-config "token_endpoint")
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
                     ]

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
