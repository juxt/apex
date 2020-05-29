;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.examples.cms.cms
  (:require
   [juxt.apex.alpha.cms.core :as cms]
   [crux.api :as crux]
   [integrant.core :as ig]))

(defmethod ig/init-key ::router [_ {:keys [crux-node store] :as opts}]
  (assert store)
  (cms/make-router
   (reify
     cms/ApexBackend
     (handle-request [_ ctx req respond raise]
       (let [body (slurp (:body req))]
         (crux/submit-tx
          crux-node
          [[:crux.tx/put
            {:crux.db/id (java.net.URI. "https://juxt.pro/frontpage3.css")
             :crux.web/content-type "text/css;charset=utf-8"
             :crux.web/content body
             :crux.ac/classification :public}]])
         (respond {:status 201 :body "Uploaded!\n"}))))
   opts))
