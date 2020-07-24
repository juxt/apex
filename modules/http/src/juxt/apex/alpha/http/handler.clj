;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.alpha.http.handler
  (:require
   [clojure.string :as str]
   [juxt.apex.alpha.http.core :as http]
   [juxt.apex.alpha.http.conditional :refer [wrap-precondition-evalution]]
   [juxt.apex.alpha.http.ring :as ring]))

(defn request-url
  "Return the full URL of the request. Copied from Ring core 1.8.0, to avoid
  adding a dependency on Ring."
  [request]
  (str (-> request :scheme name)
       "://"
       (get-in request [:headers "host"])
       (:uri request)
       (if-let [query (:query-string request)]
         (str "?" query))))

(defn effective-uri [request]
  (java.net.URI. (request-url request)))

(defn wrap-lookup-resource [h provider]
  (fn [request respond raise]
    (try
      (if (satisfies? http/ResourceLocator provider)
        (if-let [resource (http/lookup-resource provider (effective-uri request))]
          ;; Continue the chain, but with the resource assoc'd
          (h (assoc request :juxt.http/resource resource) respond raise)
          ;; The resource was not found, we exit the middleware chain with a 404
          (respond {:status 404 :headers {}}))
        ;; The will be no assoc'd resource on the request, we continue and let
        ;; the provider determine the response. It is unlikely, outside of
        ;; testing and simple demos, that a provider will not satisfy
        ;; http/ResourceLocator
        (h request respond raise))
      (catch Exception e
        (raise e)))))

(defn wrap-server-options [h provider]
  (fn [request respond raise]
    (h
     request
     (fn [response]
       (try
         (let [server
               (when (satisfies? http/ServerOptions provider)
                 (http/server-header provider))]
           (respond
            (cond-> response
              server (assoc-in [:headers "server"] server))))
         (catch Throwable t
           (raise
            (ex-info
             "Error in server-header function"
             {}
             t)))))
     raise)))

(defn invoke-method [provider]
  (fn [request respond raise]
    (try
      (http/http-method
       provider
       (:juxt.http/resource request)
       request
       respond
       raise)
      (catch Throwable t
        (raise
         (ex-info
          (format
           "Error on %s of %s"
           (str/upper-case (name (:request-method request)))
           (:uri request))
          {:request request}
          t))))))

(defn handler [provider]
  (->
   (invoke-method provider)
   (wrap-server-options provider)
   (wrap-precondition-evalution provider)
   (wrap-lookup-resource provider)
   ring/sync-adapt))
