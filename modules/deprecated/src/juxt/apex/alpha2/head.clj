;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.alpha2.head
  (:require
   [juxt.apex.alpha2.conditional-requests :as condreq]))

(defn implicit-head-method
  "Note: the Ring core middleware wrap-head calls into the handler. This
  is an optimized version which does not."
  [resource]
  {:head
   {:handler
    (fn
      ([req] {:status 200})
      ([req respond raise]
       (respond {:status 200})))
    :middleware
    [
     ;; TODO: Add wrap-coerce-parameters with GET's
     ;; definition
     [condreq/wrap-conditional-request (:apex/validators resource)]
     ]}})
