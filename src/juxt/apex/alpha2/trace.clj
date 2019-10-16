;; Copyright © 2019, JUXT LTD.

(ns juxt.apex.alpha2.trace
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

(defn trace? [req]
  (when-let [qs (some-> req :query-string codec/url-decode)]
    (not-empty (filter #(.equals "trace" %) (str/split qs #"&")))))

(defn trace-response [req]
  {:status 200
   :headers {"content-type" "text/html;charset=utf-8"}
   :body
   (html/content-from-template
    (slurp
     (io/resource "juxt/apex/alpha2/trace.html"))
    (merge
     {"style" (delay (slurp (io/resource "juxt/apex/style.css")))
      "footer" (delay (slurp (io/resource "juxt/apex/footer.html")))}

     ;;default-template-map

     {"method" (.toUpperCase (name (get req :request-method)))
      "uri" (get req :uri)
      "request"
      (html/map->table req)

      #_"summary-table"
      #_(html/vec->table
         [[:operation (get-in req [:reitit.core/match :data (:request-method req) :apex/operation "operationId"])]]
         )

      "parameters"
      (html/vec->table
       [{:head "name"
         :get first
         :render str
         :style identity}
        {:head "description"
         :get (comp (getter "description") :param second)
         :render str
         :style identity}
        {:head "in"
         :get (constantly "query")
         :render str
         :style identity}
        {:head "required"
         :get (comp (getter "required") :param second)
         :render str
         :style identity}
        {:head "style"
         :get (comp (getter "style") :param second)
         :render str
         :style identity}
        {:head "explode"
         :get (comp (getter "explode") :param second)
         :render str
         :style identity}
        {:head "schema"
         :get (comp (getter "schema") :param second)
         :render str
         :style identity}
        {:head "raw-values"
         :get (comp :raw-values second)
         :render str
         :style identity}
        {:head "validation"
         :get (comp :validation second)
         :render str
         :style identity}
        {:head "value"
         :get (comp :value second)}
        ]

       (seq (get-in req [:apex/parameters :query :apex/params]))

       #_(map
          (juxt
           first
           (comp (getter "description") :param second)
           (comp (getter "required") :param second)
           (comp (getter "explode") :param second)
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

(def wrap-trace
  {:name "Trace console"
   :compile
   (fn [data opts]
     (fn [h]
       (fn
         ([req]
          (if (trace? req)
            (trace-response req)
            (h req)))
         ([req respond raise]
          (if (trace? req)
            (respond (trace-response req))
            (h req respond raise))))))})
