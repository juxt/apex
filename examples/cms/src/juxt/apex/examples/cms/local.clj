;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.examples.cms.local
  (:require
   [juxt.apex.alpha.cms.core :as cms]
   [integrant.core :as ig]
   [clojure.edn :as edn]
   [juxt.apex.examples.cms.content :as content]
   [clojure.java.io :as io]))

;; This is for development, but will be quite quickly be replaced by a
;; local Crux database for development once things are stable.

(defrecord LocalContentStore [entities]
  cms/ContentStore
  (lookup-resource [_ id]
    (get entities id)))

(defmethod ig/init-key ::content-store [_ _]
  (println "Creating local content store")
  (->>
   (content/content-txes)
   (map (juxt :crux.db/id identity))
   (into {})
   (new LocalContentStore)))
