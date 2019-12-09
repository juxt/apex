;; Copyright © 2019, JUXT LTD.

(ns juxt.apex.alpha.redoc.redoc
  (:require
   [clojure.tools.logging :as log]
   [clojure.java.io :as io]
   [jsonista.core :as jsonista]
   [comb.template :as template]))

(defn- redoc-response [openapi-doc-path]
  {:status 200
   :body (template/eval
          (slurp (io/resource "juxt/apex/alpha/redoc/redoc.html"))
          {:openapi-doc openapi-doc-path})})

(defn new-redoc-handler [openapi-doc-path]
  (fn
    ([req]
     (redoc-response openapi-doc-path))
    ([req respond raise]
     (respond (redoc-response openapi-doc-path)))))
