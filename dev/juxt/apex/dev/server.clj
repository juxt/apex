;; Copyright © 2019, JUXT LTD.

(ns juxt.apex.dev.server
  (:require
   [ring.adapter.jetty :as jetty]
   [juxt.apex.alpha2.openapi
    :refer [compile-handler]]
   [integrant.core :as ig]
   [clojure.java.io :as io]
   [reitit.core :as r]
   [juxt.apex.yaml :as yaml]
   [reitit.ring :as ring]))

(def database
  (atom {"1" {"name" "Sven" "type" "Dog"}
         "2" {"name" "Luna" "type" "Cat"}
         "3" {"name" "Arya" "type" "Cat"}
         "4" {"name" "Kaia" "type" "Cat"}
         "5" {"name" "Vega" "type" "Dog"}}))

(defn create-ring-handler []

  (let [doc (yaml/parse-string
             (slurp
              (io/resource "juxt/apex/openapi-examples/petstore-expanded.yaml")))]
    (compile-handler
     doc
     {:apex/add-implicit-head? true
      :apex/resources
      {"/pets"
       {:apex/methods
        {:get
         {:handler
          (fn [req callback raise]
            (callback {:status 200
                       :body (vals @database)}))

          }}
        }}
      }))

  #_(ring/ring-handler
     (ring/router
      [["/{*path}"
        {:name :pets
         :handler handler
         :middleware
         [:clean-response
          [:server-header "JUXT Apex"]
          ]}]

       #_["/index.html"
          (fn [req respond _]
            (respond {:status 200 :body (io/input-stream (io/resource "public/index.html"))}))]

       #_["/assets/react/{*path}"
          (ring/create-resource-handler
           {:root "META-INF/resources/webjars/react/16.8.5"
            :parameter :path})]

       #_["/assets/react-dom/{*path}"
          (ring/create-resource-handler
           {:root "META-INF/resources/webjars/react-dom/16.8.5"
            :parameter :path})]

       #_["/assets/react-jsonschema-form/{*path}"
          (ring/create-resource-handler
           {:root "META-INF/resources/webjars/react-jsonschema-form/1.0.5"
            :parameter :path})]

       #_["/js/{*path}"
          (ring/create-resource-handler
           {:root "public"
            :parameter :path})]

       ]

      {:reitit.middleware/registry
       {:clean-response clean-response-middleware
        :server-header server-header-middleware

        }})))

;((create-ring-handler) {:uri "/" :request-method :get})

(defmethod ig/init-key :juxt.apex.dev.server/jetty
  [_ {:keys [handler]
      :juxt.apex.dev/keys [new-handler-on-each-request?]
      :as opts}]

  (jetty/run-jetty
   (if new-handler-on-each-request?
     (fn [req respond raise]
       (let [h (create-ring-handler)]
         (h req respond raise)
         ;;(respond {:status 200 :body "OK\n"})
         ))

     (create-ring-handler))
   (-> opts (dissoc :handler)
       (assoc :join? false
              :async? true
              :async-timeout 5000))))

(defmethod ig/halt-key! :juxt.apex.dev.server/jetty [_ server]
  (.stop server))
