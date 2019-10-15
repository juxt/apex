(ns juxt.apex.alpha2.response)

(defn wrap-server-header [h server]
  (fn [req respond raise]
    (h req (fn [response]
             (respond (assoc-in response [:headers "server"] server)))
       raise)))

(def server-header-middleware
  {:name "Add Server response header"
   :description "Sets a server header"
   :wrap wrap-server-header})
