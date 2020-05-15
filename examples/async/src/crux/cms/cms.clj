(ns crux.cms.cms
  (:require
   [crux.api :as crux]
   [selmer.parser :as selmer]
   [selmer.util :refer [*custom-resource-path*]]
   [integrant.core :as ig]))

(defn uri [req]
  (format "%s://%s%s"
          (-> req :scheme name)
          (-> req :headers (get "host"))
          (-> req :uri)))

(defprotocol ContentStore
  (find-entity [_ id] "Find the entity with the given id"))

(defn handle-entity [ent req respond raise]
  (cond
    (:crux.web/redirect ent)
    (respond {:status 302 :headers {"location" (str (:crux.web/redirect ent))}})

    :else
    (respond {:status 200 :body "TODO: Render entity"})
    )
  )

(defn make-router [{:keys [store]}]
  (fn [req respond raise]
    (if-let [ent (find-entity store (java.net.URI. (uri req)))]
      (handle-entity ent req respond raise)

      #_(cond
          (:content e) {:status 200 :body (:content e)}
          (:template e)
          {:status 200
           :body
           (binding [*custom-resource-path*
                     (java.net.URL. "http://localhost:8000/templates/")]
             (selmer/render-file
              (.toURL (:template e)) (dissoc e :template)
              :custom-resource-path (java.net.URL. "http://localhost:8000/templates/")))}
          :else {:status 400 :body "TODO"})
      (respond {:status 404 :body "Crux CMS: 404 (Not found)"}))))

(defmethod ig/init-key ::router [_ opts]
  (make-router opts))
