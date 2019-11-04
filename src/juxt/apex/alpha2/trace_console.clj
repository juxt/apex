;; Copyright © 2019, JUXT LTD.

(ns juxt.apex.alpha2.trace-console
  (:require
   [clojure.data :refer [diff]]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.pprint :refer [pprint]]
   [juxt.apex.alpha2.html :as html]
   [juxt.apex.alpha2.openapi :as openapi]
   [juxt.apex.alpha2.trace :as trace]
   [juxt.apex.alpha2.util :refer [fast-get-in]]
   [juxt.apex.yaml :as yaml]
   [reitit.core :as r]
   [ring.util.codec :as codec]))

(def default-template-map
  {"style" (delay (slurp (io/resource "juxt/apex/style.css")))
   "footer" (delay (slurp (io/resource "juxt/apex/footer.html")))})

(defn getter
  "A function that is more ergonmic to use in comp"
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

(defn section [title description body]
  (let [anchor (codec/url-encode (str/replace (.toLowerCase title) #"\s+" "--"))]
    {:title title
     :anchor anchor
     :description description
     :content
     (html/content-from-template
      (slurp
       (io/resource "juxt/apex/alpha2/section.html"))
      {"title" title
       "body" body
       "description" description
       "anchor" anchor})}))

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

(defn toc [sections]
  (str
   "<table>"
   (apply str
          (for [{:keys [title anchor description]} sections]
            (format "<tr><td><a href=\"#%s\">%s</a></td><td>%s</td></tr>" anchor title description)
            ))
   "</table>"))

(defn requests-index [req params request-history-atom]
  ;; TODO: I feel this :apex/params level is too much - remove the
  ;; :apex/params level.
  (let [limit (fast-get-in params [:query "limit" :value] 10)
        sections
        [(section
          "Requests"
          "An index of all incoming requests, over time"
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
          "A form that can be used to control this page"
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
           "</form method=\"GET\">"))]]

    {:status 200
     :headers {"content-type" "text/html;charset=utf-8"}
     :body (html/content-from-template
            (slurp
             (io/resource "juxt/apex/alpha2/trace-console.html"))
            (merge
             (template-model-base (:reitit.core/router req))
             {"title" "Requests Index"
              "toc" (toc sections)

              #_"debug"
              #_(debug {:router (reitit.core/options (:reitit.core/router req))})
              "body"
              (apply str (map :content sections)
               )
              }))}))

(defn to-url [req]
  (cond-> (str (name (:scheme req)) ":")
    true (str "//" (:server-name req) )
    (or
     (and (= (:scheme req) :http) (not= (:server-port req) 80))
     (and (= (:scheme req) :https) (not= (:server-port req) 443)))
    (str ":" (:server-port req))
    true (str (:uri req))
    (:query-string req) (str "?" (:query-string req))))

(comment
  (to-url {:scheme :https :server-port 443 :server-name "localhost" :uri "/" }))

(defn request-trace [req params request-history-atom]
  (let [index (fast-get-in params [:path "requestId" :value])
        item (get @request-history-atom index)
        journal (:apex/request-journal item)
        journal-entries-by-trace-id (group-by :apex.trace/middleware journal)
        sections
        [(section
          "Summary"
          "A summary table of the inbound request prior to processing."
          (html/map->table
           (first (get journal-entries-by-trace-id trace/wrap-trace-inner))
           {:sort identity
            :order [:request-method :uri :query-string :headers :scheme :server-name :server-port :remote-addr :body]}))

         (section
          "Middleware Trace"
          "An ordereed trace of the Ring middleware steps involved in processing the request."
          (delay
            (html/vec->table
             [{:head "Middleware"
               :get (comp :name :apex.trace/middleware)
               :render str
               :style identity}
              {:head "Index"
               :get :index
               :link
               (fn [row v]
                 (format
                  "<a href=\"%s\">%s</a>"
                  (href (:reitit.core/router req) (str "/requests/" index "/states/" v))
                  v))
               :render (partial index-number-format 2)
               :style identity}
              {:head "Contributions"
               :get
               (fn [x]
                 (second
                  (diff
                   (dissoc x :apex.trace/next-request-state :apex.trace/middleware)
                   (dissoc (:apex.trace/next-request-state x) :apex.trace/middleware :apex.trace/next-request-state))))
               :render str}]
             (remove
              (comp ::trace/trace-middleware :apex.trace/middleware)
              journal))))

         ;; TODO: Extract this elsewhere to an extension mechanism
         (section
          "Query Parameters"
          "The results of extracting parameters encoded into the query string of the request."
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
          "The results of extracting parameters encoded into the path of the request."
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
          "Pre-processed Request"
          "A dump of the entire request state, prior to processing."
          (html/map->table (dissoc (first (get journal-entries-by-trace-id trace/wrap-trace-outer)) :apex.trace/next-request-state)))

         (section
          "Post-processed Request"
          "A dump of the final request state, after middleware processing and prior to calling the handler, if appropriate."
          (html/map->table (dissoc (first (get journal-entries-by-trace-id trace/wrap-trace-inner)) :apex.trace/next-request-state)))
         ]]
    {:status 200
     :headers {"content-type" "text/html;charset=utf-8"}
     :body (html/content-from-template
            (slurp
             (io/resource "juxt/apex/alpha2/trace-console.html"))
            (merge
             (template-model-base (:reitit.core/router req))
             {"title" "Request Trace"
              "toc" (toc sections)
              "jumbo" (to-url (first (get journal-entries-by-trace-id trace/wrap-trace-outer)))
              "body"
              (apply str (map :content sections))}))}))

(defn request-state-trace [req params request-history-atom]
  (let [index (fast-get-in params [:path "requestId" :value])
        item (get @request-history-atom index)
        journal (:apex/request-journal item)
        state-index (fast-get-in params [:path "requestId" :value])
        state (get journal state-index)
        sections
        [(section
          "Summary"
          "A summary table of the request state"
          (html/map->table
           state
           {:sort identity
            :order [:request-method :uri :query-string :headers :scheme :server-name :server-port :remote-addr :body]}))

         ]]
    {:status 200
     :headers {"content-type" "text/html;charset=utf-8"}
     :body (html/content-from-template
            (slurp
             (io/resource "juxt/apex/alpha2/trace-console.html"))
            (merge
             (template-model-base (:reitit.core/router req))
             {"title" "Request State"
              "body"
              (apply str (map :content sections))}))}))

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
                (catch Exception e (raise e)))))})}}

      "/requests/{requestId}/states/{stateId}"
      {:apex/methods
       {:get
        (let [response
              (fn [req]
                (request-state-trace req (:apex/params req) request-history-atom))]
          {:handler
           (fn
             ([req] (response req))
             ([req respond raise]
              (try
                (respond (response req))
                (catch Exception e (raise e)))))})}}}})))
