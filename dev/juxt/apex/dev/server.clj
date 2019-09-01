(ns juxt.apex.dev.server
  (:require
   [ring.adapter.jetty :as jetty]
   [integrant.core :as ig]
   [clojure.java.io :as io]
   [juxt.apex.dev.api :as api]
   [reitit.core :as r]
   [reitit.ring :as ring]))

(io/resource "config.edn")

(io/resource "META-INF/resources/webjars/react/16.8.5/index.js")


#_(r/match-by-name
 (ring/router
  [["/api/{*path}" {:name :pets
                    :handler nil}]

   ["/assets/ping2"
    (fn [req respond _]
      (respond {:status 200
                :body "Hello Mal!"}))]
   ])
 :pets
 {:path "foo"}
 )

(defmethod ig/init-key :juxt.apex.dev.server/jetty [_ {:keys [handler] :as opts}]
  (jetty/run-jetty
   (ring/ring-handler
    (ring/router
     [["/api/{*path}" {:name :pets
                       :handler handler}]

      ["/index.html"
       (fn [req respond _]
         (respond {:status 200 :body (io/input-stream (io/resource "public/index.html"))}))]

      ["/assets/react/{*path}"
       (ring/create-resource-handler
        {:root "META-INF/resources/webjars/react/16.8.5"
         :parameter :path})]

      ["/assets/react-dom/{*path}"
       (ring/create-resource-handler
        {:root "META-INF/resources/webjars/react-dom/16.8.5"
         :parameter :path})]

      ["/assets/react-jsonschema-form/{*path}"
       (ring/create-resource-handler
        {:root "META-INF/resources/webjars/react-jsonschema-form/1.0.5"
         :parameter :path})]

      ["/js/{*path}"
       (ring/create-resource-handler
        {:root "public"
         :parameter :path})]

      ]))
   (-> opts (dissoc :handler)
       (assoc :join? false
              :async? true
              :async-timeout 1000))))

(defmethod ig/halt-key! :juxt.apex.dev.server/jetty [_ server]
  (.stop server))
