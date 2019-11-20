;; Copyright © 2019, JUXT LTD.

(ns juxt.apex.alpha2.jwt
  (:require
   [clojure.java.io :as io])
  (:import
   (com.nimbusds.jose.jwk JWKSet)
   (com.nimbusds.jose.crypto RSASSAVerifier ECDSAVerifier)
   (com.nimbusds.jwt SignedJWT)))

(defn jwks
  "Load a JWKS from a source, such as a File, InputStream, KeyStore or
  URL."
  [source]
  (JWKSet/load source))

(defn lookup-key [jwks kid]
  (.getKeyByKeyId jwks kid))

(defn signed-jwt
  "From an Base64-encoded JWT, create a signed JWT object."
  [s]
  (SignedJWT/parse s))

(defn validate-jws
  "Return true if the given JWS is valid with respect to the given
  signing key."
  [jws jwks]
  (let [kid (.. jws getHeader getKeyID)
        jwk (lookup-key jwks kid)
        verifier
        (case
            (.getValue (.getKeyType jwk))
            "RSA" (new RSASSAVerifier jwk)
            "EC" (new ECDSAVerifier jwk))]
    (.verify jws verifier)))

(defn claims [id-token]
  (into {} (.. id-token getJWTClaimsSet getClaims))
  )

#_(valid-jwt?
   (signed-jwt "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCIsImtpZCI6IkpSY080bnhzNWpnYzhZZE43STJoTE80Vl9xbDFiZG9pTVhtY1lnSG00SHMifQ.eyJzdWIiOiI0OTgwMTEwOSIsImVtYWlsIjoibWFsQGp1eHQucHJvIiwicHJlZmVycmVkX3VzZXJuYW1lIjoibWFsIiwibmFtZSI6Ik1hbGNvbG0gU3BhcmtzIiwiYXRfaGFzaCI6IlhTeXZXTUNucTJDSmY2ZW01MERNaEEiLCJzaWQiOiJkYTM2NGNjOS0zZTQzLTRkZDctOWE5OS03ODFjMjFkMDA5ZTMiLCJhdWQiOiJlZDZkZDc5MC1lZTJiLTAxMzYtZGY1Ny0wYTE2M2U2N2I4NGMxNDAxMzkiLCJleHAiOjE1NzQxNzg5NTIsImlhdCI6MTU3NDE3MTc1MiwiaXNzIjoiaHR0cHM6Ly9vcGVuaWQtY29ubmVjdC5vbmVsb2dpbi5jb20vb2lkYyJ9.xCe1pYm7n9Lc0DF_mXJwMG0Jap3C4T7bRjHtMRZt45litbUAdsbiyiG194Y5sVmWn82hdNiWubbEC4MbVP6rf1C1JmR9xUtLKLBXTNUoJkRpuSBdJafMgKUKrRSiqjoFEClrJ2evk_6owBkG8if9HTldZJQ2bxK9hIRmkvp62Ha4D0Q-7xyNXvfkwcQX7tSxE1Wd5ro26Hpne4iIqFlJfS_ZlX_H_CQ9-XZ65Xc3kSj1gMPrM2HsVS1HSuiVrguWX0kJu7PjYf6dOwIC6o2VfR7rYp4v5m-3p-r47uoFNMmVOYgTxn0YjjmaH0zTOpXwLK6zXgi3ccarOcO559yVFw")
   (jwks (io/file "jwks-juxt-dev.json"))
   )



(comment
  (valid-jwt?
   (signed-jwt "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCIsImtpZCI6IkpSY080bnhzNWpnYzhZZE43STJoTE80Vl9xbDFiZG9pTVhtY1lnSG00SHMifQ.eyJzdWIiOiI0OTgwMTEwOSIsImVtYWlsIjoibWFsQGp1eHQucHJvIiwicHJlZmVycmVkX3VzZXJuYW1lIjoibWFsIiwibmFtZSI6Ik1hbGNvbG0gU3BhcmtzIiwiYXRfaGFzaCI6IkRScEM2TWxHN0VzOUlobEVmYllYb2ciLCJzaWQiOiIwZDI4NDFhNy04M2Y3LTQ2MjktOWQ3MC0wNDU2NjY4NTViODAiLCJhdWQiOiJlZDZkZDc5MC1lZTJiLTAxMzYtZGY1Ny0wYTE2M2U2N2I4NGMxNDAxMzkiLCJleHAiOjE1NzM4NDk4MDUsImlhdCI6MTU3Mzg0MjYwNSwiaXNzIjoiaHR0cHM6Ly9vcGVuaWQtY29ubmVjdC5vbmVsb2dpbi5jb20vb2lkYyJ9.quk-GAq7COH2xfBrz4Z86g8BtIwStVTbSmF6_4uiw1msxN0nwuHS8hePZjtDvGiJC44WbNPxXgiOBON1ZWrGmKLuSDWHOTYCngiKqk7hgLPw12300uxu6TExD-5R-vT6ZG3VVHXBUGaNjopYbnQ78ZfLpq7CFLi9h0i2M6F0G7thH1XWw_sUgleY_jZz8czTZA5fWigD0n1l7MkohsiLDmWO6_fpKE9NYcSvL7yxRWbV_D3EKy91qDCeH9TnPke-w2GGFYDPqbWHKLeQ0kChf8iZeiuNmWin-G9jCfw7pnWjikOi_IFZsnBJ80pZUkt4qokgwOwUr2d3Sh_ptpRYPg")

   (jwks (io/file "jwks-juxt-dev.json"))
   ))

;; EC - keep this code as it shows how one could form a test - just
;; not sure we need a test for this logic as it's a simple wrapper on
;; Nimbus.

(comment
  (let [ec-jwk
        (..
         (new com.nimbusds.jose.jwk.gen.ECKeyGenerator com.nimbusds.jose.jwk.Curve/P_256)
         (keyID "123")
         generate)
        ec-jwk-public (.toPublicJWK ec-jwk)
        signer (new com.nimbusds.jose.crypto.ECDSASigner ec-jwk)
        claims-set (..
                    (new com.nimbusds.jwt.JWTClaimsSet$Builder)
                    (subject "alice")
                    (issuer "https://c2id.com")
                    (expirationTime (java.util.Date. (+  (.getTime (new java.util.Date)) (* 60 1000))))
                    (build)
                    )
        signed-jwt (new com.nimbusds.jwt.SignedJWT
                        (..
                         (new com.nimbusds.jose.JWSHeader$Builder com.nimbusds.jose.JWSAlgorithm/ES256)
                         (keyID (.getKeyID ec-jwk))
                         build)
                        claims-set)
        _ (.sign signed-jwt signer)
        s  (.serialize signed-jwt)
        signed-jwt-2 (SignedJWT/parse s)
        verifier (new com.nimbusds.jose.crypto.ECDSAVerifier ec-jwk-public)
        ]

    (.verify signed-jwt verifier)

    ;;(.getKeyType ec-jwk)


    ))
