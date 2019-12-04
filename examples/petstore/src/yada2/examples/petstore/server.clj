;; Copyright © 2019, JUXT LTD.

(ns yada2.examples.petstore.server
  (:require
   [clojure.java.io :as io]
   [integrant.core :as ig]
   [yada2.alpha.openapi.openapi :as openapi]
   [yada2.alpha.openapi.yaml :as yaml]
   ;;[yada2.alpha.trace.trace-console :as console]
   ;;[juxt.apex.util :refer [ring-handler]]
   [reitit.core :as r]
   reitit.middleware
   [reitit.ring :as ring]
   [ring.adapter.jetty :as jetty]))

(def database
  (atom {"1" {"name" "Sven" "type" "Dog"}
         "2" {"name" "Luna" "type" "Cat"}
         "3" {"name" "Arya" "type" "Cat"}
         "4" {"name" "Kaia" "type" "Cat"}
         "5" {"name" "Vega" "type" "Dog"}}))

(defn create-root-router [{:apex/keys [request-history-atom] :as opts}]
  (ring/router
   [["/hello"
     {:name :hello
      :get {:handler
            (fn
              ([req] {:status 200 :body "Hello\n"})
              ([req respond raise]
               (respond {:status 200 :body (str "Hello, requests so far: " (count @request-history-atom) "\n")})))}}]

    (openapi/create-api-route
     "/pets-api"
     (yaml/parse-string
      (slurp
       (io/resource "juxt/apex/openapi-examples/petstore-expanded.yaml")))
     (merge
      opts
      {:name :pets-api}
      {:apex/add-implicit-head? false
       :apex/resources
       {"/pets"
        {:apex/methods
         {:get
          {:handler
           (let [response
                 (fn [req]
                   (let [limit (get-in req [:apex/params :query "limit" :value])]
                     (throw (ex-info "Forced exception" {:data 123
                                                         :type :forced}))
                     {:status 200
                      :body (str (vec (cond->> (vals @database) limit (take limit))) "\n")}))]
             (fn
               ([req] (response req))
               ([req respond raise]
                (respond (response req)))))}}}}}))

    #_(console/trace-console opts)]))

(defn create-root-handler
  ([] (create-root-handler {:apex/request-history-atom (atom [])}))
  ([opts]
   (ring/ring-handler
    (create-root-router opts)

    (ring/create-default-handler
     {:not-found
      (let [not-found-response
            {:status 404
             :headers {"content-type" "text/plain"}
             :body "Apex: Not found\n"}]
        (fn
          ([req] not-found-response)
          ([req respond raise] (respond not-found-response))))}))))


#_(assert
 (= {:status 200
     :body "Hello\n"}
    ((create-root-handler) {:uri "/hello" :request-method :get})))

#_(assert
 (=
  {:status 200
   :body "[{\"name\" \"Sven\", \"type\" \"Dog\"} {\"name\" \"Luna\", \"type\" \"Cat\"}]\n"}
  ((create-root-handler) {:uri "/pets-api/pets"
                          :query-string "limit=2"
                          :request-method :get})))

#_((create-root-handler) {:uri "/pets-api/pets"
                          :query-string "limit=2"
                          :request-method :get})

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

      }}))

(defmethod ig/init-key ::jetty
  [_ {:keys [new-handler-on-each-request?]
      :as opts}]

  (let [request-history-atom (atom [])]
    (jetty/run-jetty
     (if new-handler-on-each-request?
       (fn [req respond raise]
         (let [h (create-root-handler {:apex/request-history-atom request-history-atom})]
           (h req respond raise)))
       ;; Production
       (create-root-handler {:apex/request-history-atom request-history-atom}))
     (-> opts (dissoc :handler)
         (assoc :join? false
                :async? true
                :async-timeout 5000)))))

(defmethod ig/halt-key! ::jetty [_ server]
  (.stop server))
