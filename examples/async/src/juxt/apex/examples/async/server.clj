;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.examples.async.server
  (:require
   [integrant.core :as ig]
   [jsonista.core :as jsonista]
   reitit.middleware
   [reitit.ring :as ring]
   reitit.ring.middleware.dev
   [ring.adapter.jetty :as jetty]))

(defn create-root-router
  [_]
  (ring/router
   [
    ["/index"

     {:handler
      (fn this
        ([req]
         (this req identity #(throw %)))
        ([req respond _]
         (respond
          {:status 200
           :headers {"content-type" "application/json"}
           :body (str
                  (jsonista/write-value-as-string
                   {"message" "Hello World!"
                    "body" (slurp (:body req))})
                  "\r\n")})))}]]
   {}))

(defn create-root-handler
  ([opts]
   (ring/ring-handler
    (create-root-router opts)

    (ring/create-default-handler
     {:not-found
      (let [not-found-response
            {:status 404
             :headers {"content-type" "text/plain"}
             :body "server: Not found\n"}]
        (fn
          ([_] not-found-response)
          ([_ respond _] (respond not-found-response))))}))))

(defmethod ig/init-key ::jetty
  [_ {:keys [juxt.apex.examples/listener-port]
      :as opts}]

  (jetty/run-jetty
   (create-root-handler {})
   (-> opts
       (dissoc :handler)
       (assoc :port listener-port
              :join? false
              :async? true
              :async-timeout 5000))))

(defmethod ig/halt-key! ::jetty [_ server]
  (.stop server))
