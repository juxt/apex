;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.alpha.http.handler
  (:require
   [clojure.string :as str]
   [juxt.apex.alpha.http.core :as http]
   [juxt.apex.alpha.http.resource :as resource]
   [juxt.apex.alpha.http.server :as server]
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

(defn wrap-lookup-resource [h resource-provider]
  (fn [request respond raise]
    (try
      (if (satisfies? resource/ResourceLocator resource-provider)
        (if-let [resource (http/lookup-resource resource-provider (effective-uri request))]
          ;; Continue the chain, but with the resource assoc'd
          (h (assoc request :juxt.http/resource resource) respond raise)
          ;; The resource was not found, we exit the middleware chain with a 404
          (respond {:status 404}))
        ;; The will be no assoc'd resource on the request, we continue and let
        ;; the resource-provider determine the response. It is unlikely, outside of
        ;; testing and simple demos, that a resource-provider will not satisfy
        ;; http/ResourceLocator
        (h request respond raise))
      (catch Exception e
        (raise e)))))

(defn wrap-server-options [h server]
  (fn [request respond raise]
    (h
     request
     (fn [response]
       (try
         (let [server
               (when (satisfies? server/ServerOptions server)
                 (server/server-header server))]
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

(defn- invoke-method [resource-provider server]
  (fn [request respond raise]
    (let [resource (:juxt.http/resource request)
          method (:request-method request)]
      (if-not (find (or (:juxt.http/methods resource) #{:get :head}) method)
        ;; Method Not Allowed!
        (respond {:status 405})
        ;; Proceed to invoke method...
        (try
          (http/http-method resource-provider server resource request respond raise)
          (catch Throwable t
            (raise
             (ex-info
              (format
               "Error on %s of %s"
               (str/upper-case (name method))
               (:uri request))
              {:request request}
              t))))
        ))))

(defn handler [resource-provider server]
  (->
   (invoke-method resource-provider server)
   (wrap-precondition-evalution resource-provider)
   (wrap-lookup-resource resource-provider)
   (wrap-server-options server)
   ring/sync-adapt))
