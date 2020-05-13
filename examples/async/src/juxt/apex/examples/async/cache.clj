;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.examples.async.cache)

(defn wrap-cache [handler opts]
  (fn [req respond raise]
    (handler
     (update req :headers conj ["cache-middleware" "yes"])
     respond raise)))

(def cache-example
  (->
   (fn [req respond raise]
     (println "cache-middleware header?" (get-in req [:headers "cache-middleware"]))
     (respond {:status 200
               :headers {"example" "cache-example"}
               :body "TODO: Replace this with some stream\n"}))
   (wrap-cache {})))
