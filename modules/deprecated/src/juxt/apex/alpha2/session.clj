;; Copyright © 2019, JUXT LTD.

(ns juxt.apex.alpha2.session
  (:require
   [ring.middleware.session :as session])
  )

(def session-middleware
  {:name "Session"

   :wrap session/wrap-session
   #_:compile
   #_(fn [_ _]
     (println "WRAP SESSION")
     session/wrap-session
     #_(fn [h]
       (fn
         ([req] (h req))
         ([req respond raise] (h req respond raise)))))})
