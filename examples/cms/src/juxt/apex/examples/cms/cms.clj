;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.examples.cms.cms
  (:require
   [juxt.apex.alpha.cms.core :as cms]
   [crux.api :as crux]
   [integrant.core :as ig]))

(defmethod ig/init-key ::router [_ {:keys [crux-node] :as opts}]
  (cms/make-router
   (assoc
    opts
    :callback
    (fn [req respond raise]
      (let [body (slurp (:body req))]
        (crux/submit-tx
         crux-node
         [[:crux.tx/put
           {:crux.db/id (java.net.URI. "https://juxt.pro/frontpage3.css")
            :crux.web/content-type "text/css;charset=utf-8"
            :crux.web/content body
            :crux.ac/classification :public}]]))
      (respond {:status 201 :body "Uploaded!\n"})))))
