(ns juxt.warp.dev.server
  (:require
   [ring.adapter.jetty :as jetty]
   [integrant.core :as ig]
   [juxt.warp.dev.api :as api]))

(defmethod ig/init-key :juxt.warp.dev.server/jetty [_ {:keys [handler] :as opts}]
  (jetty/run-jetty
   handler
   (-> opts (dissoc :handler)
       (assoc :join? false
              :async? true
              :async-timeout 1000))))

(defmethod ig/halt-key! :juxt.warp.dev.server/jetty [_ server]
  (.stop server))
