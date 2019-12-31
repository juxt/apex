;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.alpha.trace.trace-console
  (:require
   [clojure.data :refer [diff]]
   [clojure.java.io :as io]
   [clojure.pprint :refer [pprint]]
   [clojure.string :as str]
   [reitit.core :as r]
   [ring.util.codec :as codec]
   [reitit.ring :as ring]
   [juxt.apex.alpha.openapi.yaml :as yaml]
   [juxt.apex.alpha.params.parameters :as params]
   [juxt.apex.alpha.trace.html :as html]
   [juxt.apex.alpha.trace.trace :as trace]))

;; Could be duplicated - if changing this, check for other instances
(defmacro fast-get-in
  "In his ClojuTre 2019 talk, Tommi Riemann says that `->` is
  signifantly faster than `get-in`."
  ([m args]
   `(fast-get-in ~m ~args nil))
  ([m args not-found]
   `(let [res# (-> ~m ~@(for [i args] (if (keyword? i) `~i `(get ~i))))]
      (if res# res# ~not-found))))

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
       (io/resource "juxt/apex/alpha/trace/section.html"))
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
  (if (number? n)
    (.format
     (doto
         (java.text.NumberFormat/getIntegerInstance)
       (.setMinimumIntegerDigits max-digits)
       (.setGroupingUsed false))
     n)
    ""))

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

(defn template-model-base []
  {"style" (delay (slurp (io/resource "juxt/apex/alpha/trace/style.css")))
   "footer" (delay (slurp (io/resource "juxt/apex/alpha/trace/footer.html")))})

(defn toc [sections]
  (str
   "<table>"
   (apply str
          (for [{:keys [title anchor description]} sections]
            (format "<tr><td><a href=\"#%s\">%s</a></td><td>%s</td></tr>" anchor title description)
            ))
   "</table>"))

(defn navbar [items]
  (str
   "<ul>"
   "<li>apex</li>"
   (apply str (for [{:keys [title href]} items]
                (str "<li><a href=\"" href "\">" title "</a></li>")))
   "</ul>"))

(defn requests-index [req params request-history-atom]
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
                       (href (:reitit.core/router req) (str "/traces/requests/" (:index row)))
                       v))
             :render (partial index-number-format 4)}
            {:head "date"
             :get (comp html/render-date :apex/start-date)
             :render str
             :style html/monospace}
            {:head "uri"
             :get (comp :uri :apex.trace/request-state first :apex/request-journal)
             :render str
             :style html/monospace}
            {:head "query-string"
             :get (comp :query-string :apex.trace/request-state first :apex/request-journal)
             :render str
             :style html/monospace}]
           (cond->>
               (reverse @request-history-atom)
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
             (io/resource "juxt/apex/alpha/trace/trace-console.html"))
            (merge
             (template-model-base)
             {"title" "Requests Index"
              "toc" (toc sections)
              "navbar" (navbar [])

              #_"debug"
              #_(debug {:router (reitit.core/options (:reitit.core/router req))})

              "body"
              (apply str (map :content sections))}))}))

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
  (assert req)
  (assert params)
  (let [index (fast-get-in params [:path "requestId" :value])
        item (get @request-history-atom index)
        journal (:apex/request-journal item)
        journal-entries-by-trace-id (group-by :apex.trace/middleware journal)
        journal-entries-by-type (group-by :type journal)
        _ (def item item)
        sections
        [
         (section
          "Summary"
          "Overall status of request"
          "")

         (section
          "Request"
          "A summary table of the inbound request prior to processing."
          (html/map->table
           (:apex.trace/request-state (first (get journal-entries-by-trace-id trace/wrap-trace-inner)))
           {:sort identity
            :order [:request-method :uri :query-string :headers :scheme :server-name :server-port :remote-addr :body]
            :dynamic (fn [k v]
                       (if (= k :request-method)
                         {:render (comp html/default-render str/upper-case name)}
                         {:render html/default-render}))}))

         (section
          "Processing Steps"
          "A trace of the steps involved in processing the request."
          (delay
            (html/vec->table
             [{:head "Index"
               :get :index
               :link
               (fn [row v]
                 (format
                  "<a href=\"%s\">%s</a>"
                  (href (:reitit.core/router req) (str "/traces/requests/" index "/states/" v))
                  v))
               :render (partial index-number-format 2)
               :style identity}
              {:head "Step"
               :get (comp :name :apex.trace/middleware)
               :render str
               :style identity}
              {:head "Type"
               :get :type
               :render name
               :style identity}
              {:head "Contributions"
               :dynamic
               (fn [x]
                 (case (:type x)
                   :enter
                   {:render str
                    :get
                    (fn [x] (second
                             (diff
                              (:apex.trace/request-state x)
                              (-> x :apex.trace/next-request-state :apex.trace/request-state))))}
                   :exception-from-handler
                   {:render str
                    :get (:exception x)}

                   {:render str
                    :get (fn [x] "")}))
               }]
             journal
             )))

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

             (seq (get-in (first (get journal-entries-by-type :invoke-handler))
                          [:apex.trace/request-state :apex/params :query])))))

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

             (seq (get-in (first (get journal-entries-by-type :invoke-handler))
                          [:apex.trace/request-state :apex/params :path])))))

         (section
          "Pre-processed Request"
          "A dump of the entire request state, prior to processing."
          (html/map->table (:apex.trace/request-state (first (get journal-entries-by-trace-id trace/wrap-trace-outer))))
          )

         (section
          "Post-processed Request"
          "A dump of the final request state, after middleware processing and prior to calling the handler, if appropriate."
          (html/map->table (:apex.trace/request-state (first (get journal-entries-by-trace-id trace/wrap-trace-inner)))))
         ]]
    {:status 200
     :headers {"content-type" "text/html;charset=utf-8"}
     :body (html/content-from-template
            (slurp
             (io/resource "juxt/apex/alpha/trace/trace-console.html"))
            (merge
             (template-model-base)
             {"title" "Request Trace"
              "navbar" (navbar
                        [{:title "All requests"
                          :href (href (:reitit.core/router req) "/traces/requests")}])
              "toc" (toc sections)

              ;; TODO: Why does this fail?
;;              "jumbo" (to-url (:apex.trace/request-state (first (get journal-entries-by-trace-id trace/wrap-trace-outer))))

              "body"
              (apply str (map :content sections))
              }))}))

(defn request-state-trace [req params request-history-atom]
  (let [index (fast-get-in params [:path "requestId" :value])
        item (get @request-history-atom index)
        journal (:apex/request-journal item)
        state-index (fast-get-in params [:path "stateId" :value])
        state (get-in journal [state-index :apex.trace/request-state])
        sections
        [(section
          "Summary"
          "A summary table of the request state"
          (html/map->table
           state
           {:sort identity
            :order [:request-method :uri :query-string :headers :scheme :server-name :server-port :remote-addr :body]}))]]
    {:status 200
     :headers {"content-type" "text/html;charset=utf-8"}
     :body (html/content-from-template
            (slurp
             (io/resource "juxt/apex/alpha/trace/trace-console.html"))
            (merge
             (template-model-base)
             {"title" "Request State"
              "navbar"
              (navbar
               [{:title "All requests"
                 :href (href (:reitit.core/router req) "/requests")}
                {:title (index-number-format 4 index)
                 :href (href (:reitit.core/router req) (str "/requests/" index))}])
              "body"
              (apply str (map :content sections))}))}))

(defn trace-console [{:apex/keys [request-history-atom] :as opts}]
  (let [openapi (yaml/parse-string
                 (slurp
                  (io/resource "juxt/apex/alpha/trace/traces.yaml")))]
    ["/traces"
     ["/requests"
      (let [openapi-operation (get-in openapi ["paths" "/requests" "get"])]
        {:get (fn this
                ([req] (this req identity #(throw %)))
                ([req respond raise]
                 (try
                   (respond (requests-index req (:apex/params req) request-history-atom))
                   (catch Exception e
                     (raise e)))))
         :middleware
         [
          [params/openapi-parameters-middleware (get openapi-operation "parameters")]]})
      ]

     ["/requests/{requestId}"
      (let [openapi-operation (get-in openapi ["paths" "/requests/{requestId}" "get"])]
        {:get (fn this
                ([req] (this req identity #(throw %)))
                ([req respond raise]
                 (try
                   (respond (request-trace req (:apex/params req) request-history-atom))
                   (catch Throwable e
                     (raise e)))))
         :middleware
         [
          [params/openapi-parameters-middleware (get openapi-operation "parameters")]]})
      ]

     ["/requests/{requestId}/states/{stateId}"
      (let [openapi-operation (get-in openapi ["paths" "/requests/{requestId}/states/{stateId}" "get"])]
        {:get (fn this
                ([req] (this req identity #(throw %)))
                ([req respond raise]
                 (try
                   (respond (request-trace req (:apex/params req) request-history-atom))
                   (catch Exception e
                     (raise e)))))
         :middleware
         [
          [params/openapi-parameters-middleware (get openapi-operation "parameters")]]})
      ]])

  #_(openapi/create-api-route
     "/traces"
     (yaml/parse-string
      (slurp
       (io/resource "juxt/apex/alpha/trace/traces.yaml")))
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
