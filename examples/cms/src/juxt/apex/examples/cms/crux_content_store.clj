;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.examples.cms.crux-content-store
  (:require
   [juxt.apex.alpha.cms.core :as cms]
   [crux.api :as crux]
   [integrant.core :as ig]))

(defrecord CruxContentStore [node]
  cms/ContentStore
  (lookup-resource [_ uri]
    (crux/entity (crux/db node) uri))
  (propfind [this uri depth]
    (let [uris
          (map
           first
           (crux/q
            (crux/db node)
            '{:find [e]
              :where [(or-join [e] [e :crux.web/content-source] [e :crux.web/content])]}))]
      (into
       {}
       (for [uri
             (cms/find-members uri depth uris)]
         [uri (cms/lookup-resource this uri)])))))


(defmethod ig/init-key ::content-store [_ {:keys [node]}]
  (new CruxContentStore node))
