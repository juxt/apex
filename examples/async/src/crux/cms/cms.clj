(ns crux.cms.cms
  (:require
   [crux.api :as crux]
   [selmer.parser :as selmer]
   [integrant.core :as ig]))

(defn uri [req]
  (format "%s://%s%s"
          (-> req :scheme name)
          (-> req :headers (get "host"))
          (-> req :uri)))

(defn make-router [{:keys [crux/node]}]
  (fn [req respond raise]
    (respond {:status 200 :body "CMS\n"})
    #_(try
      (let [db (crux/db node)]

        (if-let [e (crux/entity db (java.net.URI. (uri req)))]

          (cond
            (:content e) {:status 200 :body (:content e)}
            (:template e)
            (do
              (println "template is" (:template e))
              {:status 200
               :body
               (binding [selmer.util/*custom-resource-path*
                         (java.net.URL. "http://localhost:8000/templates/")]
                 (selmer/render-file
                  (.toURL (:template e)) (dissoc e :template)
                  :custom-resource-path (java.net.URL. "http://localhost:8000/templates/")))})
            :else {:status 400 :body "TODO"})
          {:status 404 :body "Not found"}))
      (catch Exception e
        (println e)
        {:status 500 :body "ERROR: Check the logs"}))))

(defmethod ig/init-key ::router [_ opts]
  (make-router opts))
