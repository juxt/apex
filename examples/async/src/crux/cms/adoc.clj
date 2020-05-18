(ns crux.cms.adoc
  (:require [clojure.reflect :refer [reflect]]
            [clojure.java.io :as io])
  (:import (org.asciidoctor Asciidoctor$Factory Options Attributes SafeMode))
  )

(defn engine []
  (Asciidoctor$Factory/create))

(defn block-seq
  "Return all the blocks in a depth-first search of the given document"
  [doc]
  (tree-seq
    (fn branch? [node]
      (or
       (instance? org.asciidoctor.ast.Document node)
       (instance? org.asciidoctor.ast.Section node)
       (instance? org.asciidoctor.ast.Block node)
       ))
    (fn children [node] (.getBlocks node))
    doc))

(defmulti ->crux-entity type)

(defmethod ->crux-entity org.asciidoctor.ast.Document [doc]
  (merge
   (into {} (.getAttributes doc))
   {:crux.db/id (java.net.URI. "https://juxt.pro/_sources/site/index.adoc")
    :asciidoctor/type :document}))

(defmethod ->crux-entity org.asciidoctor.ast.Section [section]
  (let [attributes (.getAttributes section)]
    (merge
     (into {} attributes)
     {:crux.db/id (java.net.URI. (format "https://juxt.pro/_sources/site/index.adoc#%s" (get attributes "id" (str (java.util.UUID/randomUUID)))))
      :asciidoctor/type :section}
     (when (get attributes "id") {:crux.cms/content (.getContent section)}))))

(defmethod ->crux-entity org.asciidoctor.ast.Block [block]
  (let [attributes (.getAttributes block)]
    (merge
     (into {} attributes)
     {:crux.db/id (java.net.URI. (format "https://juxt.pro/_sources/site/index.adoc#%s" (get attributes "id" (str (java.util.UUID/randomUUID)))))
      :asciidoctor/type :block}
     (when (get attributes "id") {:crux.cms/content (.getContent block)}))))

(defmethod ->crux-entity :default [node]
  {:crux.db/id (java.net.URI. "https://juxt.pro/_sources/site/index.adoc#methodology")
   :asciidoctor/type (str (type node))})




#_(def doc (.loadFile
          (engine)
          (io/file "/home/malcolm/src/github.com/juxt/plan/site/index.adoc")
          {"safe" org.asciidoctor.SafeMode/UNSAFE
           "header_footer" false
           "to_file" false
           "backend" "html5"
           "template_dirs" [(.getAbsolutePath (io/file "resources/adoc_backend/html5"))]
           "attributes" {"sectanchors" true
                         "figure-caption" false
                         "icons" "font"}}))


#_(for [block
      (block-seq doc)]
  (->crux-entity block)

  )
