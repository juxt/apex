;; Copyright © 2019, JUXT LTD.

(ns juxt.apex.alpha.redoc.redoc
  (:require
   [clojure.tools.logging :as log]
   [clojure.java.io :as io]
   [jsonista.core :as jsonista]))

;; TODO: Include comb "0.1.1"

(defn- redoc-response []
  {:status 200
   :body (slurp (io/resource "juxt/apex/alpha/redoc/redoc.html"))})

(defn new-redoc-handler [doc]
  (fn
    ([req]
     (redoc-response))
    ([req respond raise]
     (respond (redoc-response)))))
