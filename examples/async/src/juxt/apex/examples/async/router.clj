;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.examples.async.router
  (:require
   [juxt.apex.examples.async.cache :as cache]
   [juxt.apex.examples.async.flowables :as flowables]
   [juxt.apex.examples.async.upload :as upload]
   [juxt.apex.examples.async.rs :as rs]
   [juxt.apex.examples.async.sse :as sse]
   [integrant.core :as ig]))

(defn router [opts req respond raise]
  (condp re-matches (:uri req)

    #"/upload-file"
    (upload/upload-file-example opts req respond raise)

    #"/flow"
    (flowables/flow-example opts req respond raise)

    #"/bp"
    (rs/backpressure-example opts req respond raise)

    #"/cache-example"
    (cache/cache-example req respond raise)

    ;; SSE
    #"/sse" (sse/sse-example req respond raise)

    #"/ticker" (flowables/ticker-example opts req respond raise)

    (respond {:status 404})))

(defmethod ig/init-key ::router [_ _]
  #'router)
