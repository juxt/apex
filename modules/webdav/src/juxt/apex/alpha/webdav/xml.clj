;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.alpha.webdav.xml
  (:refer-clojure :exclude [-> = descendants]))

;; See https://juxt.pro/blog/posts/xpath-in-transducers.html if you're baffled by the below

(def content (mapcat :content))

(defn tagp [pred]
  (comp content (filter (comp pred :tag))))

(defn tag= [tag]
  (tagp (partial clojure.core/= tag)))

(defn content= [s]
  (comp content (filter (partial clojure.core/= s))))

(defn attrs [a]
  (comp a :attrs))

(defn attrp [a pred]
  (filter (comp pred (attrs a))))

(defn attr [name]
  (comp (map :attrs) (map (fn [x] (get x name)))))

(defn attr= [a v]
  (attrp a (partial clojure.core/= v)))

(declare ->)

(defn = [path arg]
  (filter #(clojure.core/= (apply -> % path) arg)))

(def xml-seq*
  (fn [rf]
    (fn
      ([] (rf))
      ([result] (rf result))
      ([result input]
       (rf result (xml-seq input))))))

(def descendants (comp content xml-seq* cat))

(defn apply-path [el & path]
  (sequence (apply comp path) [el]))

(defn shorthand [x]
  (let [->1 (fn [el path] (comp first (sequence (apply comp (map shorthand path)) [el])))]
    (cond
      (keyword? x) (tag= x)
      (vector? x) (let [[op path & args] (first x)]
                    (filter #(apply op (apply ->1 % path) args))
                    )
      :else x)))

(defn ->*
  "Takes an xml/parse nested associated structure and applies some transactors"
  [el & path]
  (apply apply-path el (map shorthand path)))

(defn -> [& args]
  (first (apply ->* args)))
