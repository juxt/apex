(ns crux.cms.crux-content-store
  (:require [crux.cms.cms :as cms]
            [crux.api :as crux]
            [integrant.core :as ig]))

(defrecord CruxContentStore [node]
  cms/ContentStore
  (find-entity [_ id]
    (crux/entity (crux/db node) id)))

(defmethod ig/init-key ::content-store [_ {:keys [node]}]
  (new CruxContentStore node))
