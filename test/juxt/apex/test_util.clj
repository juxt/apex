;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.test-util)

(defn call-handler [handler request]
  (let [p (promise)]
    (handler
     request
     (fn [response] (deliver p response))
     (fn [err] (deliver p err)))
    p))
