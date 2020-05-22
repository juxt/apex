;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.examples.cms.adoc
  (:require [clojure.java.io :as io]
            [integrant.core :as ig])
  (:import (org.asciidoctor Asciidoctor$Factory)))

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
     (when (get attributes "id") {:crux.cms/content (.getContent section)
                                  :crux.cms/bookmark? true}))))

(defmethod ->crux-entity org.asciidoctor.ast.Block [block]
  (let [attributes (.getAttributes block)]
    (merge
     (into {} attributes)
     {:crux.db/id (java.net.URI. (format "https://juxt.pro/_sources/site/index.adoc#%s" (get attributes "id" (str (java.util.UUID/randomUUID)))))
      :asciidoctor/type :block}
     (when (get attributes "id") {:crux.cms/content (.getContent block)
                                  :crux.cms/bookmark? true}))))

(defmethod ->crux-entity :default [node]
  nil
  #_{:crux.db/id (java.net.URI. (format "https://juxt.pro/_sources/site/index.adoc#%s" (str (java.util.UUID/randomUUID))))
   :asciidoctor/type (str (type node))})


(defn load-content [asciidoctor content]
  (.load
   asciidoctor
   content
   {"header_footer" false
    "to_file" false
    "backend" "html5"
    "template_dirs" [(.getAbsolutePath (io/file "resources/adoc_backend/html5"))]
    "attributes" {"sectanchors" true
                  "figure-caption" false
                  "icons" "font"}}))

(defn extract-bookmarked-content [doc]
  (filter
   (some-fn :crux.cms/bookmark? #(= (:asciidoctor/type %) :document))
   (for [block
         (block-seq doc)]
     (->crux-entity block))))

(defn template-model [engine content]
  (let [doc (load-content engine content)]
    (apply merge
           (for [block (block-seq doc)]
             (let [attributes (into {} (.getAttributes block))]
               (cond
                 (instance? org.asciidoctor.ast.Document block)
                 attributes

                 (get attributes "id")
                 {(get attributes "id")
                  (merge attributes
                         {"content" (.getContent block)})}))))))


(template-model
  (engine)
  (slurp (io/file "/home/malcolm/src/github.com/juxt/plan/site/index.adoc")))

(defmethod ig/init-key ::engine [_ _]
  (engine))
