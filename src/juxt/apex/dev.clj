(ns juxt.apex.dev
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

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
            (str/replace
             (slurp (io/resource "juxt/apex/404.html"))
             "{{routes}}"
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
                        (get op "summary"))))))))}))
         (respond response)))
     raise)))
