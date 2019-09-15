;; Copyright © 2019, JUXT LTD.

(ns juxt.apex.alpha2.head
  (:require
   [juxt.apex.alpha2.conditional-requests :as condreq]))

(defn implicit-head-method
  "Note: the Ring core middleware wrap-head calls into the handler. This
  is an optimized version which does not."
  [resource {:apex/keys [handler-middleware-transform]}]
  {:head
   {:handler (fn [req respond raise] (respond {:status 200}))
    :middleware ((or handler-middleware-transform (fn [_ mw] mw))
                 resource
                 [
                  ;; TODO: Add wrap-coerce-parameters with GET's
                  ;; definition
                  [condreq/wrap-conditional-request (:apex/validators resource)]
                  ])}})
