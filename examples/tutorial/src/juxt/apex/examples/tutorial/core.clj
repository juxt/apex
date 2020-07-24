;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.examples.tutorial.core
  (:require
   [integrant.core :as ig]
   [juxt.apex.alpha.http.core :as http]
   [juxt.apex.alpha.http.handler :as handler]))

(defrecord Provider [site-map]

  http/ResourceLocator
  (locate-resource [this uri]
    (get site-map (.getPath uri)))

  http/Resource
  (invoke-method
    [this resource response request respond raise]
    (case (:request-method request)
      :head (respond response)
      :get
      (respond
       {:status 200
        :body (:juxt.http/content resource)})))

  http/PostMethod
  (POST
    [_ ctx request respond raise]
    (respond
     {:status 200
      :body (:juxt.http/content resource)})
    )
  )

;; TODO: GETs with etags

(def site-map
  {"/hello" {:juxt.http/content "Hello World!"}})

(defn handler [req respond raise]
  (let [h (handler/handler (->Provider site-map))]
    (h req respond raise)))

(defmethod ig/init-key ::handler [_ _]
  #'handler)
