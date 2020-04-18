(ns juxt.apex.examples.async.vertx
  (:require
   [integrant.core :as ig]
   [jsonista.core :as jsonista]
   reitit.middleware
   [reitit.ring :as ring]
   reitit.ring.middleware.dev
   [ring.adapter.jetty :as jetty]))

(defmethod ig/init-key ::listener
  [_ {:keys [juxt.apex.examples/listener-port]
      :as opts}]
  (println "Starting Vert.X")
  )

(defmethod ig/halt-key! ::listener [_ server]
  (println "Stopped Vert.X")
  )
