;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.examples.cms.sse
  (:require
   [juxt.apex.examples.cms.flowable :as f]))

(defn sse-example [_ respond _]
  (respond
   {:status 200
    :headers {"content-type" "text/event-stream"}
    :body (->>
           (f/range 1 10000)
           ;;(f/throttle-first 100 TimeUnit/MILLISECONDS)
           ;;(#(.timestamp %))
           (#(.limit % 50))
           (#(.materialize %))
           ;;           (#(.dematerialize %))
           (f/map f/server-sent-event))}))
