(ns juxt.apex.alpha2.middleware.exception)

(defn exception-handler-request
  ([request]
   (exception-handler-request request {}))
  ([request options]
   request))

(defn exception-handler-response
  ([response]
   (exception-handler-response response {}))
  ([response options]
   response))

(defn wrap-exception-handler
  "Ensures that any errors are caught and handled
  appropriately. Integrates with trace console where possible."
  ([handler]
   (wrap-exception-handler {}))
  ([handler options]
   (fn
     ([request]
      (-> request
          (exception-handler-request options)
          handler
          (exception-handler-response options)))
     ([request respond raise]
      (handler (exception-handler-request request options)
               (fn [response] (respond (exception-handler-response response options)))
               raise)))))
