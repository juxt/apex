;; Copyright © 2020, JUXT LTD.
;;

(ns juxt.apex.examples.petstore.customer-client
  (:require
   [integrant.core :as ig]
   [ring.adapter.jetty :as jetty]))

(defn create-root-handler
  ([opts]
   (fn
     ([req] {:status 200 :body "TODO"})
     ([req respond raise] (respond {:status 200 :body "TODO"})))))

(defmethod ig/init-key ::jetty
  [_ {:keys [juxt.apex.examples/listener-port
             juxt.apex.examples.client/auth-config
             juxt.apex.examples.client/cookie-name]
      :as opts}]

  (let [request-history-atom (atom [])
        session-opts
        {:store (ring.middleware.session.memory/memory-store (atom {}))
         :cookie-name cookie-name
         }]
    (jetty/run-jetty
     (create-root-handler
      (merge
       auth-config
       {:apex/request-history-atom request-history-atom
        :apex/session-opts session-opts}))
     (-> opts
         (dissoc :handler)
         (assoc :port listener-port
                :join? false
                :async? true
                :async-timeout 5000)))))

(defmethod ig/halt-key! ::jetty [_ server]
  (.stop server))
