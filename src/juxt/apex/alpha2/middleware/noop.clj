;; Intended to be copied

(ns juxt.apex.alpha2.middleware.noop)

(defn noop-request
  ([request]
   (noop-request request {}))
  ([request options]
   request))

(defn noop-response
  ([response]
   (noop-response response {}))
  ([response options]
   response))

(defn wrap-noop
  "Ensures that any errors are caught and handled
  appropriately. Integrates with trace console where possible."
  ([handler]
   (wrap-noop {}))
  ([handler options]
   (fn
     ([request]
      (-> request
          (noop-request options)
          handler
          (noop-response options)))
     ([request respond raise]
      (handler
       (noop-request request options)
       (fn [response] (respond (noop-response response options)))
       raise)))))
