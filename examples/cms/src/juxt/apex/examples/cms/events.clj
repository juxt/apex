;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.examples.cms.events
  (:require
   [jsonista.core :as jsonista]
   [juxt.apex.alpha.vertx.helpers :refer [h]]
   [integrant.core :as ig])
  )

(defmethod ig/init-key ::stock-feed-publisher
  [_ {:keys [topic vertx freq]}]
  (let [eb (.eventBus vertx)
        price (atom 100)
        timer-id
        (.setPeriodic
         vertx freq
         (h
          (fn [_]
            (.publish
             eb topic
             (jsonista/write-value-as-string
              {"equity" topic
               "price" (swap! price + (rand) -0.5)})))))]
    (printf "Initialising publisher on topic %s (timer-id: %d)\n" topic timer-id)
    {:topic topic
     :timer-id timer-id
     :close! #(.cancelTimer vertx timer-id)}))

(defmethod ig/halt-key! ::stock-feed-publisher [_ {:keys [topic close! timer-id]}]
  (printf "Cancelling publisher on topic %s (timer-id: %d)\n" topic timer-id)
  (close!))
