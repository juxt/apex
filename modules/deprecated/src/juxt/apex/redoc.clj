;; Copyright © 2019, JUXT LTD.

(ns juxt.apex.redoc
  (:require
   [clojure.tools.logging :as log]
   [clojure.java.io :as io]
   [jsonista.core :as jsonista]))

(defn wrap-redoc [h api options]
  (fn [req respond raise]
    (cond
      (= (:uri req) "/")
      (respond {:status 200
                :body (slurp (io/resource "juxt/apex/redoc.html"))})

      (= (:uri req) "/swagger.json")
      (do
        (respond {:status 200
                  :headers {"content-type" "application/json"}
                  :body (jsonista/write-value-as-string
                         (assoc
                          (:apex/doc options)
                          "servers"
                          [{"url" "http://localhost:8080/"}]))
                  }))

      :else
      (h req respond raise))))
