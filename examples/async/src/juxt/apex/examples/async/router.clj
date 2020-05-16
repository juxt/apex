;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.examples.async.router
  (:require
   [juxt.apex.examples.async.cache :as cache]
   [juxt.apex.examples.async.flowables :as flowables]
   [juxt.apex.examples.async.upload :as upload]
   [juxt.apex.examples.async.rs :as rs]
   [juxt.apex.examples.async.sse :as sse]
   [integrant.core :as ig]))

(defn make-router [cms-router]
  (fn [req respond raise]
    (condp re-matches (:uri req)

      #"/upload-file"
      (upload/upload-file-example req respond raise)

      #"/flow"
      (flowables/flow-example req respond raise)

      #"/bp"
      (rs/backpressure-example req respond raise)

      #"/cache-example"
      (cache/cache-example req respond raise)

      ;; SSE
      #"/sse" (sse/sse-example req respond raise)

      #"/ticker" (flowables/ticker-example req respond raise)

      (cms-router req respond raise))))

(defmethod ig/init-key ::router [_ {:keys [cms-router]}]
  (assert cms-router)
  (make-router cms-router))
