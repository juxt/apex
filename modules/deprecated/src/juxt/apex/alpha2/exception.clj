(ns juxt.apex.alpha2.exception)

;; See duplication in juxt.apex.alpha2.middleware.exception

(defn exception-response [e]
  {:status 500
   ;; TODO: implement
   :body "ERROR (TODO: expand this with link to trace)"})

(defn wrap [handler]
  (fn
    ([request]
     (try
       (handler request)
       (catch Throwable e
         (exception-response e)
         )))
    ([request respond raise]
     (try
       (handler request respond (fn [e] (exception-response e)))
       (catch Throwable e
         (exception-response e))))))

(def exception-middleware
  {:name "Exception"
   :wrap wrap})
