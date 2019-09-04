;; Copyright © 2019, JUXT LTD.

(ns juxt.apex.dev
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [juxt.apex.format :as format]
   [muuntaja.core :as m]))

(def handlebars-pattern #"\{\{([^\}]*)\}\}")

(defn process-content [content {:keys [status title error]} api]
  (str/replace
   content
   handlebars-pattern
   (fn [[_ s]]
     (case s
       "status" (str status)
       "title" title
       "style" (slurp (io/resource "juxt/apex/style.css"))
       "footer" (slurp (io/resource "juxt/apex/footer.html"))

       "routes"
       (format
        "<table style='border: 1px solid black; border-collapse: collapse'>
<thead><tr><th>Path</th><th>Method</th><th>Summary</th></tr></thead>
<tbody>%s</tbody></table>"
        (str/join
         (map (partial format "<tr>%s<tr>")
              (for [[path pi] (-> api (get "paths"))
                    [method op] pi]
                (str
                 (format
                  "<td>%s</td>"
                  (if (and (= method "get") (= -1 (.indexOf path "{")))
                    (format "<a href='%s'>%s</a>" path path)
                    path))
                 (format
                  "<td>%s</td>"
                  (.toUpperCase method))
                 (format
                  "<td>%s</td>"
                  (get op "summary")))))))
       "error" error))))

(defn wrap-helpful-404 [h api]
  (fn [req respond raise]
    (h
     req
     (fn [response]
       (if (= (:status response) 404)
         (respond
          (merge
           response
           {:headers
            {"content-type" "text/html;charset=utf-8"}
            :body
            (process-content
             (slurp (io/resource "juxt/apex/404.html"))
             (merge response {:title "Not Found"})
             api)}))
         (respond response)))
     raise)))

(defn wrap-helpful-error [h api]
  (fn [req respond raise]
    (let [m (m/create
             (-> m/default-options
                 (assoc :formats {"text/html" (format/text-format "text/html")})
                 (dissoc :default-format)))]

      (try
        (let [format (m/response-format m req)]
          (if (= (:format format) "text/html")
            (h
             req
             respond
             ;; Only do this if we can neg text/html
             (fn [error]
               (respond
                {:status 500
                 :headers
                 {"content-type" "text/html;charset=utf-8"}
                 :body
                 ;; TODO: Render the error
                 (process-content
                  (slurp (io/resource "juxt/apex/error.html"))
                  {:status 500
                   :title "Internal Server Error"
                   :error (pr-str error)}

                  api)})))

            ;; No text/html
            (h req respond raise)))
        (catch Exception e
          (h req respond raise)
          )))))
