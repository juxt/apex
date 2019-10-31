;; Copyright © 2019, JUXT LTD.

(ns juxt.apex.alpha2.trace-console
  (:require
   [clojure.data :refer [diff]]
   [clojure.java.io :as io]
   [clojure.pprint :refer [pprint]]
   [juxt.apex.alpha2.html :as html]
   [juxt.apex.alpha2.openapi :as openapi]
   [juxt.apex.alpha2.trace :as trace]
   [juxt.apex.alpha2.util :refer [fast-get-in]]
   [juxt.apex.yaml :as yaml]
   [reitit.core :as r]))

(def default-template-map
  {"style" (delay (slurp (io/resource "juxt/apex/style.css")))
   "footer" (delay (slurp (io/resource "juxt/apex/footer.html")))})

(defn getter
  ([k]
   (fn [x] (get x k)))
  ([k default]
   (fn [x] (get x k default))))

#_(defn trace? [req]
  (when-let [qs (some-> req :query-string codec/url-decode)]
    (not-empty (filter #(.equals "trace" %) (str/split qs #"&")))))

#_(defn trace-response [req]
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

      "summary-table"
      (let [method (:request-method req)]
        (html/vec->table
         [{:get first :render html/kw->name :style identity}
          {:get second}]
         [[:operation
           (get-in req [::r/match :data method :apex/operation "operationId"])]]))


      "creation-form"
      (delay
        (let [doc (get-in req [::r/match :data :post :apex/openapi])]
          (json/write-value-as-string
           (some->
            (get-in req [::r/match :data :post :apex/operation "requestBody" "content" "application/json" "schema"
                         ])
            (resolve-json-ref {:base-document doc})
            first
            ))))


      "parameters"
      (delay
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
          {:head "encoded-strings"
           :get (comp :encoded-strings second)
           :render str
           :style identity}
          {:head "validation"
           :get (comp :validation second)
           :render str
           :style identity}
          {:head "value"
           ;; TODO: Try get :error
           :get (comp (fn [{:keys [value error]}]
                        (or value error))
                      second)}]

         (seq (get-in req [:apex/params :query :apex/params]))))

      "raw-request"
      (delay
        (html/escape
         (with-out-str
           (pprint req))))}))})

#_(defn trace-console-handler
  ([req]
   (trace-response req))
  ([req respond raise]
   (respond (trace-response req))))

(defn section [title body]
  (html/content-from-template
   (slurp
    (io/resource "juxt/apex/alpha2/section.html"))
   {"title" title
    "body" body}))

(defn debug [body]
  (html/content-from-template
   (slurp
    (io/resource "juxt/apex/alpha2/section.html"))
   {"title" "Debug"
    "body" (str
            "<pre style='font-family: monospace'>"
            (with-out-str (pprint body))
            "</pre>")}))

(defn style-href [uri body]
  (format "<a href=\"%s\">%s</a>" uri body))

(defn index-number-format [max-digits n]
  (.format
   (doto
       (java.text.NumberFormat/getIntegerInstance)
     (.setMinimumIntegerDigits max-digits)
     (.setGroupingUsed false))
   n))

(defn href [router path]
  ;; TODO: This could also take a third parameter, a request method
  ;; that should be supported by the match
  (let [outer-path (:path (r/options router))
        full-path (str outer-path path)]
    (if-let [match (r/match-by-path router full-path)]
      full-path
      (throw
       (ex-info (str "Invalid href: no match for path " full-path)
                {:output-path outer-path
                 :full-path full-path
                 :path path})))))

(defn template-model-base [router]
  {"style" (delay (slurp (io/resource "juxt/apex/style.css")))
   "footer" (delay (slurp (io/resource "juxt/apex/footer.html")))
   "home.href" (href router "/requests")})

(defn requests-index [req params request-history-atom]
  ;; TODO: I feel this :apex/params level is too much - remove the
  ;; :apex/params level.
  (let [limit (fast-get-in params [:query "limit" :value] 10)]
    {:status 200
     :headers {"content-type" "text/html;charset=utf-8"}
     :body (html/content-from-template
            (slurp
             (io/resource "juxt/apex/alpha2/trace-console.html"))
            (merge
             (template-model-base (:reitit.core/router req))
             {"title" "Requests Index"

              #_"debug"
              #_(debug {:router (reitit.core/options (:reitit.core/router req))})
              "body"
              (str
               (section
                "Requests"
                (html/vec->table
                 [{:head "#"
                   :get :index
                   :link
                   (fn [row v]
                     (format "<a href=\"%s\">%s</a>"
                             (href (:reitit.core/router req) (str "/requests/" (:index row)))
                             v))
                   :render (partial index-number-format 4)}
                  {:head "date"
                   :get (comp html/render-date :apex/start-date)
                   :render str
                   :style html/monospace}
                  {:head "uri"
                   :get (comp :uri first :apex/request-journal)
                   :render str
                   :style html/monospace}
                  {:head "query-string"
                   :get (comp :query-string first :apex/request-journal)
                   :render str
                   :style html/monospace}]
                 (cond->>
                     (let [requests (reverse @request-history-atom)]
                       requests)
                     limit (take limit))))

               (section
                "Control"
                (str
                 "<form method=\"GET\">"
                 (html/vec->table
                  [{:head "Name"
                    :get first
                    :render str
                    :style identity}
                   {:head "Description"
                    :get #(get-in % [1 :param "description"])
                    :render str
                    :style identity}
                   {:head "Value"
                    :get identity
                    :render identity
                    :escape identity
                    :style (fn [v]
                             (format "<input name=\"%s\" type=\"text\" value=\"%s\"></input>" (first v) (fast-get-in v [1 :value] 10)))
                    }]
                  (fast-get-in req [:apex/params :query]))

                 "<p><input type=\"submit\" value=\"Submit\"></input></p>"
                 "</form method=\"GET\">")))}))}))

(defn request-trace [req params request-history-atom]
  (let [index (fast-get-in params [:path "requestId" :value])
        item (get @request-history-atom index)
        journal (:apex/request-journal item)
        journal-entries-by-trace-id (group-by :apex.trace/middleware journal)]
    {:status 200
     :headers {"content-type" "text/html;charset=utf-8"}
     :body (html/content-from-template
            (slurp
             (io/resource "juxt/apex/alpha2/trace-console.html"))
            (merge
             (template-model-base (:reitit.core/router req))
             {"title" "Request Trace"
              "body"
              (str
               (section
                "Summary"
                (html/map->table
                 (first (get journal-entries-by-trace-id trace/wrap-trace-inner))
                 {:sort identity
                  :order [:request-method :uri :query-string :headers :scheme :server-name :server-port :remote-addr :body]}))
               (section
                "Story"
                (delay
                  (html/vec->table
                   [{:head "Middleware"
                     :get (comp :name :apex.trace/middleware)
                     :render str
                     :style identity}
                    {:head "Contributions"
                     :get (fn [x]
                            (second
                             (diff
                              (dissoc x :apex.trace/subsequent-request :apex.trace/middleware)
                              (dissoc (:apex.trace/subsequent-request x) :apex.trace/middleware :apex.trace/subsequent-request)))
                            )
                     :render str
                     }]
                   (remove
                    (comp ::trace/trace-middleware :apex.trace/middleware)
                    journal))))

               ;; TODO: Extract this elsewhere to an extension mechanism
               (section
                "Query Parameters"
                (delay
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
                    {:head "encoded-strings"
                     :get (comp :encoded-strings second)
                     :render str
                     :style identity}
                    {:head "validation"
                     :get (comp :validation second)
                     :render str
                     :style identity}
                    {:head "value"
                     ;; TODO: Try get :error
                     :get (comp (fn [{:keys [value error]}]
                                  (or value error))
                                second)}]

                   (seq (get-in (last journal) [:apex/params :query])))))

               (section
                "Path Parameters"
                (delay
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
                    {:head "encoded-strings"
                     :get (comp :encoded-strings second)
                     :render str
                     :style identity}
                    {:head "validation"
                     :get (comp :validation second)
                     :render str
                     :style identity}
                    {:head "value"
                     ;; TODO: Try get :error
                     :get (comp (fn [{:keys [value error]}]
                                  (or value error))
                                second)}]

                   (seq (get-in (last journal) [:apex/params :path])))))

               (section
                "Incoming request prior to middleware processing"
                (html/map->table (dissoc (first (get journal-entries-by-trace-id trace/wrap-trace-outer)) :apex.trace/subsequent-request)))

               (section
                "Request prior to handler after middleware processing"
                (html/map->table (dissoc (first (get journal-entries-by-trace-id trace/wrap-trace-inner)) :apex.trace/subsequent-request))))}))}))

(defn trace-console [{:apex/keys [request-history-atom] :as opts}]
  (openapi/create-api-route
   "/traces"
   (yaml/parse-string
    (slurp
     (io/resource "juxt/apex/dev/traces.yaml")))
   (merge
    opts
    {:apex/add-implicit-head? false
     :apex/resources
     {"/requests"
      {:apex/methods
       {:get
        (let [response
              (fn [req]
                (requests-index req (:apex/params req) request-history-atom))]
          {:handler
           (fn
             ([req] (response req))
             ([req respond raise]
              (try
                (respond (response req))
                (catch Exception e (raise e)))))})}}
      "/requests/{requestId}"
      {:apex/methods
       {:get
        (let [response
              (fn [req]
                (request-trace req (:apex/params req) request-history-atom))]
          {:handler
           (fn
             ([req] (response req))
             ([req respond raise]
              (try
                (respond (response req))
                (catch Exception e (raise e)))))})}}}})))
