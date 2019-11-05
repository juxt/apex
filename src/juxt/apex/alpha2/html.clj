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


(defn order-by [m ks]
  (apply array-map
         (mapcat seq
                 (for [k ks]
                   [k (get m k)]))))

(defn default-render [x]
  (cond
    (keyword? x) (name x)
    (string? x) x
    x (pr-str x)
    :else ""))

(defn map->table
  ([m] (map->table m {}))
  ([m options]
   (let [{:keys [sort order]
          :or {sort sort}}
         options]
     (el
      "table"
      (for [[k v] (if order (order-by m order) (sort m))]
        (el
         "tr"
         (el "td" (kw->str k))
         (let [{:keys [render]
                :or {render default-render}}
               (if-let [dyn (:dynamic options)]
                 (dyn k v)
                 options)]
           (el "td" (cond
                      (some-> v meta :apex.trace/hide)
                      "&lt;hidden&gt;"
                      (and (map? v) (not-empty v))
                      (map->table v)
                      :else (-> v
                                render
                                escape
                                monospace))))))))))

(defn vec->table
  [cols data]
  (try
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
           (for [x cols]
             (let [{:keys [get render style compute link escape]}
                   (if-let [d (:dynamic x)] (d row) x) ; dynamic
                   v ((or compute (fn [row v] v))
                      row
                      (if (ifn? get) (get row) get)
                      )]
               (el
                "td"
                (cond
                  (and (map? v) (not-empty v))
                  (map->table v)
                  :else
                  (-> v
                      ((or render default-render))
                      ((or escape default-escape))
                      ((if link (partial link row) identity))
                      ((or style monospace))))))))))))
    (catch Exception e
      (println e)
      (throw e)
      )))
