(ns crux.cms.selmer
  (:require [selmer.parser :as selmer]
            [integrant.core :as ig]))

(defmethod ig/init-key ::tags [_ _]
  (selmer/add-tag!
   :href
   (fn [[target & nvs] context-map]
     "")))
