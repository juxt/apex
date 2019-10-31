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

(defn render-date [d]
  (.format java.time.format.DateTimeFormatter/ISO_LOCAL_DATE_TIME
           (.toLocalDateTime
            (.atZone
             (.toInstant d)
             (java.time.ZoneId/systemDefault)))))

(defn map->table [m]
  (el
   "table"
   (for [[k v] (sort m)]
     (el
      "tr"
      (el "td" (kw->str k))
      (el "td" (cond
                 (some-> v meta :apex.trace/hide)
                 "&lt;hidden&gt;"
                 (and (map? v) (not-empty v))
                 (map->table v)
                 :else (monospace (escape (pr-str v)))))))))

(defn vec->table
  [cols data]
  (let [default-escape escape]
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
         (for [{:keys [get render style compute link escape]} cols]
           (let [v ((or compute (fn [row v] v))
                    row
                    (get row)
                    )]
             (el
              "td"
              (cond
                (and (map? v) (not-empty v))
                (map->table v)
                :else
                (-> v
                    ((or render pr-str))
                    ((or escape default-escape))
                    ((if link (partial link row) identity))
                    ((or style monospace)))))))))))))
