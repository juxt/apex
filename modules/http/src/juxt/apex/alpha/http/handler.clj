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

(defn wrap-lookup-resource [h resource-locator]
  (fn [request respond raise]
    (let [resource (http/lookup-resource resource-locator (effective-uri request))]
      (if resource
        (h (assoc request :juxt.http/resource resource) respond raise)
        (respond {:status 404 :headers {}})))))

(defn handler [provider]
  (when-not (satisfies? http/ResourceLocator provider)
    (throw
     (ex-info
      "Provider must satisfy mandatory ResourceLocator protocol"
      {:provider provider
       :protocol http/ResourceLocator})))
  (when-not (satisfies? http/ResponseBody provider)
    (throw
     (ex-info
      "Provider must satisfy mandatory ResponseBody protocol"
      {:provider provider
       :protocol http/ResponseBody})))

  (->

   (fn [request respond raise]

     (try
       (http/http-method
        provider
        (:juxt.http/resource request)
        request
        (fn [response]
          (let [server
                (when (satisfies? http/ServerOptions provider)
                  (http/server-header provider))]
            (respond
             (cond-> response
               server (assoc-in [:headers "server"] server)))))
        raise)

       (catch Throwable t
         (raise
          (ex-info
           (format
            "Error on %s of %s"
            (str/upper-case (name (:request-method request)))
            (:uri request))
           {:request request}
           t)))))


   (wrap-lookup-resource provider)
   (wrap-precondition-evalution provider)
   ring/sync-adapt
   ))
