(ns juxt.apex.alpha.auth-digest.core)

(defn wrap-auth-digest [handler]
  (fn this
    ([request]
     (this request identity (fn [t] (throw t))))

    ([request respond raise]
     (handler
      request
      (fn [response]
        (respond
         (cond-> response
           (#{401 207} (:status response))
           (assoc-in
            [:headers "www-authenticate"]
            ;; TODO: For now, we're going to clobber any existing
            ;; value, but we should really add to the existing string.
            "TODO"
            ))))
      raise))))
