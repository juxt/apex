;; Copyright © 2019, JUXT LTD.

(ns juxt.apex.alpha2.debug
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.pprint :refer [pprint]]
   [ring.util.codec :as codec]
   [juxt.apex.alpha2.html :as html]))

(def default-template-map
  {"style" (delay (slurp (io/resource "juxt/apex/style.css")))
   "footer" (delay (slurp (io/resource "juxt/apex/footer.html")))})

(defn getter
  ([k]
   (fn [x] (get x k)))
  ([k default]
   (fn [x] (get x k default))))

(defn debug-response [req]
  {:status 200
   :headers {"content-type" "text/html;charset=utf-8"}
   :body
   (html/content-from-template
    (slurp
     (io/resource "juxt/apex/alpha2/debug.html"))
    (merge
     {"style" (delay (slurp (io/resource "juxt/apex/style.css")))
      "footer" (delay (slurp (io/resource "juxt/apex/footer.html")))}

     ;;default-template-map

     {"method" (.toUpperCase (name (get req :request-method)))
      "uri" (get req :uri)
      "request"
      (html/map->table req)

      "summary-table"
      (html/vec->table
       [[:operation (get-in req [:reitit.core/match :data (:request-method req) :apex/operation "operationId"])]]
       )

      "query-parameters"
      (html/vec->table
       ["name" "description" "required" "schema" "raw-values" "validation"
        "value"
        ;;"raw"
        ]
       (map
        (juxt
         first
         (comp (getter "description" "none") :param second)
         (comp (getter "required") :param second)
         (comp (getter "schema") :param second)
         (comp :raw-values second)
         (comp :validation second)
         (comp :value second)
         ;;(comp second)
         )
        (get-in req [:apex/parameters :query :apex/params])))

      "raw-request"
      (html/escape
       (with-out-str
         (pprint req)))}))})

(defn debug-handler [operation]
  (fn
    ([req]
     (debug-response req))
    ([req respond raise]
     (respond (debug-response req)))))
