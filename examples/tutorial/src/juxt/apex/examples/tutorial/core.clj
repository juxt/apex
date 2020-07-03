;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.examples.tutorial.core
  (:require
   [integrant.core :as ig]
   [juxt.apex.alpha.http.core :as yada2]))



(defrecord Provider [site-map]
  yada2/ResourceLocator
  (locate-resource [this uri]
    (get site-map (.getPath uri)))

  yada2/ResponseBody
  (send-ok-response
    [this resource response request respond raise]
    (respond
     {:status 200
      :body (:juxt.http/content resource)})
    )
  )

;; TODO: GETs with etags

(def site-map
  {"/hello" {:juxt.http/content "Hello World!"}})

(defn handler [req respond raise]
  (let [h (yada2/handler (->Provider site-map))]
    (h req respond raise)))

(defmethod ig/init-key ::handler [_ _]
  #'handler)
