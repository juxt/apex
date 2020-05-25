;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.examples.cms.crux-content-store
  (:require
   [juxt.apex.alpha.cms.core :as cms]
   [crux.api :as crux]
   [integrant.core :as ig]
   [clojure.set :as set]))

(defrecord CruxContentStore [node]
  cms/ContentStore
  (find-entity [_ id]
    (crux/entity (crux/db node) id))
  (propfind [this uri depth]
    (let [uris
          ;; TODO: Replace with an or query
          (set/union
           (map
            first
            (crux/q
             (crux/db node)
             {:find ['id]
              :where [['e :crux.db/id 'id]
                      ['e :crux.web/content]]}))
           (map
            first
            (crux/q
             (crux/db node)
             {:find ['id]
              :where [['e :crux.db/id 'id]
                      ['e :crux.cms/content-source]]})))]
      (into
       {}
       (for [id
             (cms/find-members uri depth uris)]
         [id (cms/find-entity this id)])))))


(defmethod ig/init-key ::content-store [_ {:keys [node]}]
  (new CruxContentStore node))
