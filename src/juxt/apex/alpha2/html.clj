;; Copyright © 2019, JUXT LTD.

(ns juxt.apex.alpha2.html
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def handlebars-pattern #"\{\{([^\}]*)\}\}")

(defn escape [s]
  (str/replace
   s #"([\<\>&\"\'])"
   (fn [[_ match]]
     (case match
       "<" "&lt;"
       ">" "&gt;"
       "&" "&amp;"
       "'" "&apos;"
       "\"" "&quot;"))))

(defn content-from-template [template model]
  (str/replace
   template
   handlebars-pattern
   (fn [[_ s]]
     (let [v (model s)]
       (str (if (delay? v) @v v))))))

(defn monospace [s]
  (str "<span class=\"cell-value\">" s "</span>"))

(defmacro el [tag & content]
  `(str "<" ~tag ">" (apply str ~@(filter some? content)) "</" ~tag ">"))

(defmacro span [class & content]
  `(str "<span class=\"" ~class "\">" (apply str ~@content) "</span>"))

(defn kw->name [k]
  (str
   (when-let [n (namespace k)]
     (str n "/"))
   (name k)))

(defn kw->str [k]
  (span "keyword"
        (cond
          (string? k) (escape (pr-str k))
          (keyword? k) (str (kw->name k))
          :else (str k))))

(defn map->table [m]
  (el
   "table"
   (for [[k v] (sort m)]
     (el
      "tr"
      (el "td" (kw->str k))
      (el "td" (if (and (map? v) (not-empty v))
                 (map->table v)
                 (monospace (escape (pr-str v)))))))))

(defn vec->table
  [cols data]
  (el
   "table"
   (when (not-empty (keep :head cols))
     (el
      "thead"
      (el
       "row"
       (for [h (map :head cols)]
         (el "th" h)))))
   (el
    "tbody"
    (for [row data]
      (el
       "tr"
       (for [{:keys [get render style]} cols]
         (let [v (get row)]
           (el "td" (if (and (map? v) (not-empty v))
                      (map->table v)
                      ((or style monospace) (escape ((or render pr-str) v))))))))))))
