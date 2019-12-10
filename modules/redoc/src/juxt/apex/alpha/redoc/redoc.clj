;; Copyright © 2019, JUXT LTD.

(ns juxt.apex.alpha.redoc.redoc
  (:require
   [clojure.tools.logging :as log]
   [clojure.java.io :as io]
   [jsonista.core :as jsonista]
   [comb.template :as template]))

(defn- redoc-response [swagger-url]
  {:status 200
   :body (template/eval
          (slurp (io/resource "juxt/apex/alpha/redoc/redoc.html"))
          {:swagger-url swagger-url})})

(defn new-redoc-handler [swagger-url]
  (fn
    ([req]
     (redoc-response swagger-url))
    ([req respond raise]
     (respond (redoc-response swagger-url)))))

(defn- swagger-ui-response [swagger-url]
  {:status 200
   :body (template/eval
          (slurp (io/resource "juxt/apex/alpha/redoc/swagger-ui.html"))
          {:swagger-url swagger-url})})

(defn new-swagger-ui-handler [swagger-url]
  (fn
    ([req]
     (swagger-ui-response swagger-url))
    ([req respond raise]
     (respond (swagger-ui-response swagger-url)))))
