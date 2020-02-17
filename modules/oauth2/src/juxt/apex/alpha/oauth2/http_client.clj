;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.alpha.oauth2.http-client
  (:require
   [ring.util.codec :as codec])
  (:import
   [java.net URI]
   [java.net.http HttpClient HttpClient$Version HttpRequest HttpResponse$BodyHandlers HttpRequest$BodyPublishers]))

(defn new-client
  ([]
   (new-client {}))
  ([{:keys [connect-timeout]
      :or {connect-timeout (java.time.Duration/ofSeconds 20)}
     :as opts}]
   (.. (HttpClient/newBuilder)
       (version HttpClient$Version/HTTP_1_1)
       (connectTimeout connect-timeout)
       (build))))

(defn new-request
  ([method url]
   (new-request method url {}))
  ([method url {:keys [request-body-publisher headers] :as opts}]
   (.build
    (cond-> (HttpRequest/newBuilder)
      (= method :get) (.GET)
      (= method :delete) (.DELETE)
      headers ((fn [builder]
                 (doseq [[k v] headers]
                   (.setHeader builder k v))
                 builder))
      request-body-publisher (.method (.toUpperCase (name method)) request-body-publisher)
      true (.uri (URI/create url))))))

(defn request
  ([client method url]
   (request client method url {}))
  ([client
    method
    url
    {:keys [response-body-handler async on-success on-error]
     :or {response-body-handler (HttpResponse$BodyHandlers/ofString)}
     :as opts}]
   (let [request (new-request method url opts)]
     (if async
       (cond->
           (.sendAsync client request response-body-handler)
           on-error (. (exceptionally
                        (reify java.util.function.Function
                          (apply [_ error]
                            (println "Error:" error)
                            (on-error error)))))
           on-success (. (thenAccept
                          (reify java.util.function.Consumer
                            (accept [_ result]
                              (on-success result))))))
       (try
         (let [result (.send client request response-body-handler)]
           (if on-success (on-success result) result))
         (catch Exception e
           (if on-error (on-error e) (throw e))))))))

(defn ->www-form-urlencoded [m]
  (apply
   str
   (interpose
    "&"
    (for [[k v] m]
      (if (sequential? v)
        (apply str (interpose "&" (for [v v] (str k "=" v))))
        (str k "=" v))))))

;; Try https://developers.onelogin.com/openid-connect/inspector

#_(let [client (new-client)]
  (..
   (request
    client
    :get "https://juxt-dev.onelogin.com/oidc/token"
    {:async true
     :request-body-publisher
     (HttpRequest$BodyPublishers/ofString
      (->www-form-urlencoded
       {"grant_type" "authorization_code"
        "code" "MGMxMTIxOGMtODllNi00Y2IxLTgzMmUtNWRiOTlkN2JjNTFiuCB9pfAg6EonMv-7e_aCGitFnndOIe5gNT71hH4Ix1ImrqHk2EHrPCBNMV-l1crDC4RtS2shyDamk9myheKPkQ"
        "redirect_uri" "http://localhost:3002/onelogin/callback"
        "client_id" "ed6dd790-ee2b-0136-df57-0a163e67b84c140139"
        "client_secret" "dc8347cda5fc1bc5667d12f832c5f03bcb2134e59ec16586f247b25a5295209f"}))})

   (exceptionally
    (reify java.util.function.Function
      (apply [_ error]
        (println "Error:" error)
        )))

   (thenAccept
    (reify java.util.function.Consumer
      (accept [_ result]
        (println "Result:" result)
        (println "Headers:" (.headers result))
        (println "Body:" (.body result))
        ))
    ))
  )
