;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.examples.cms.populate
  (:require
   [crux.api :as crux]
   [integrant.core :as ig]
   [juxt.site.content :as content]))

(defmethod ig/init-key ::seeder [_ {:keys [node]}]
  (println "Seeding database")
  (crux/submit-tx
   node
   (for [tx (content/content-txes)]
     [:crux.tx/put tx])))
