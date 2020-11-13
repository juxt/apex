;; Copyright © 2020, JUXT LTD.

(ns dev
  (:require
   [io.dominic.wedge.dev :refer :all]
   [crux.api :as crux]))

(defn crux-node []
  (:juxt.apex.examples.cms.db/node system))

(defn db []
  (crux/db (crux-node)))

(defn entity [id]
  (dissoc
   (crux/entity (db) id)
   :apex/content))

(defn web-resource [id]
  (entity
   (if (.startsWith id "/")
     (java.net.URI. (str "https://juxt.pro" id))
     (java.net.URI. id))))
