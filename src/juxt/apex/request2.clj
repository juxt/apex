;; Copyright © 2019, JUXT LTD.

(ns juxt.apex.request2
  (:require
   [reitit.core :as r]
   [reitit.ring :as ring]
   [clojure.java.io :as io]
   [clojure.tools.logging :as log]
   [juxt.apex.yaml :as yaml]
   [ring.mock.request :as mock]))

;; An attempt to create individual pipelines with a Reitit structure
;; programmatically.

(def add-api-middleware
  {:name "Add API"
   :wrap (fn [h api]
           (fn [req respond raise]
             (log/trace "foo, api is" api)
             (h req respond raise)))})

(defn openapi-handler [doc]

  (let [routes
        (vec
         (for [[path path-item] (get doc "paths")]
           [path
            (apply merge
                   (for [[method operation] path-item
                         :let [method (keyword method)]]
                     {method
                      {:name (keyword (get operation "operationId"))
                       :handler (fn [req respond raise] (respond {:status 200 :body "OK"}))
                       :middleware []
                       }}))]))

        router
        (ring/router
         [""
          {:middleware
           [
            [add-api-middleware doc]
            ]}
          routes])]

    (ring/ring-handler router (ring/create-default-handler))))
