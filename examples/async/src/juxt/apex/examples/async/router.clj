;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.examples.async.router
  (:require
   [juxt.apex.examples.async.cache :refer [cache-example]]
   [juxt.apex.examples.async.handlers :as h]))

(defn router [opts req respond raise]
  (condp re-matches (:uri req)

    #"/upload-file"
    (h/upload-file-example opts req respond raise)

    #"/flow"
    (h/flow-example opts req respond raise)

    #"/bp"
    (h/backpressure-example opts req respond raise)

    #"/file.jpg"
    (h/file-serving-example req respond raise)

    #"/cache-example"
    (cache-example req respond raise)

    ;; SSE
    #"/sse" (h/sse-example req respond raise)

    #"/ticker" (h/ticker-example opts req respond raise)

    (respond {:status 404})))
