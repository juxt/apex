;; Copyright © 2019, JUXT LTD.

(ns juxt.apex.doc
  (:require
   [clojure.string :as str]))

(def extract-params-pattern #"\{([^\}]*)\}")

(defn path-for [api opId params]
  (first
   (for [[path pi] (get api "paths")
         [method {:strs [operationId]} op] pi
         :while (= operationId opId)]
     (str/replace
      path
      extract-params-pattern
      (fn [[_ param]] (str (get params param "NIL")))))))

(defn compile-path-template [path-template]
  (let [groups (map second (re-seq extract-params-pattern path-template))
        match-pattern (re-pattern
                       (str/replace
                        path-template
                        extract-params-pattern
                        (fn [[_ match]] (format "(?<%s>.*)" match))))]
    (fn [s]
      (let [matcher (re-matcher match-pattern s)]
        (when (.matches matcher)
          (into {}
                (mapv (fn [g] [g (.group matcher g)]) groups)))))))

(defn process-document [doc]
  (update-in
   doc ["paths"]
   (fn [m]
     (into {} (map (fn [[k v]] [k (with-meta v {:apex/matcher (compile-path-template k)})]) m)))))
