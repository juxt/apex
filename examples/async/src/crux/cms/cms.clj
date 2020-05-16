(ns crux.cms.cms
  (:require
   [juxt.apex.examples.async.async-helpers :as a]
   [clojure.pprint :refer [pprint]]
   [ring.middleware.params :refer [wrap-params]]
   [crux.api :as crux]
   [selmer.parser :as selmer]
   [selmer.util :refer [*custom-resource-path*]]
   [integrant.core :as ig]
   [clojure.string :as str]))

(defn uri [req]
  (format "%s://%s%s"
          (-> req :scheme name)
          (-> req :headers (get "host"))
          (-> req :uri)))

(defprotocol ContentStore
  (find-entity [_ id] "Find the entity with the given id"))

(defn entity-as-html [ent]
  (str "<pre>"
       (->
        (with-out-str (pprint ent))
        (str/replace "<" "&lt;")
        )
       "</pre>"))

(defn respond-entity [ent req respond raise]
  (respond
   {:status 200
    :headers {"content-type" "text/html"}
    :body (entity-as-html ent)}))

(defn render-entity-with-selmer-template [ent]
  (binding [*custom-resource-path*
            (java.net.URL. "http://localhost:8000/templates/")]
    (selmer/render-file
     (.toURL (:crux.cms.selmer/template ent)) (dissoc ent :template)
     :custom-resource-path (java.net.URL. "http://localhost:8000/templates/")
     )))

(defn redirect? [ent]
  (when-let [status (:crux.web/status ent)]
    (and (>= status 300) (< status 400))))

(defn respond-entity-response [ent vertx req respond raise]
  (cond
    (redirect? ent)
    (respond
     {:status (:crux.web/status ent)
      :headers {"location" (str (:crux.web/redirect ent))}})

    (string? (:crux.cms/content ent))
    (respond {:status 200
              :headers (cond-> {}
                         (:crux.web/content-type ent) (conj ["content-type" (:crux.web/content-type ent)]))
              :body (:crux.cms/content ent)})

    (:crux.cms.selmer/template ent)
    (a/execute-blocking-code
     vertx
     (fn [] (render-entity-with-selmer-template ent))
     {:on-success (fn [body]
                    (respond {:status 200
                              :body body}))
      :on-failure (fn [t] (raise (ex-info "Failed to render template" {:template (:crux.cms.selmer/template ent)} t)))})

    :else
    (respond {:status 500 :body (str "<body><h2>ERROR - Not handled</h2>" (entity-as-html ent) "</body>")})))

(defn make-router [{:keys [store vertx]}]
  (->
   (fn [req respond raise]
     (let [debug (get-in req [:query-params "debug"])]

       (if-let [ent (find-entity store (java.net.URI. (uri req)))]
         (if debug
           (respond-entity ent req respond raise)
           (respond-entity-response ent vertx req respond raise))

         (respond {:status 404 :body "Crux CMS: 404 (Not found)"}))))

   wrap-params))

(defmethod ig/init-key ::router [_ opts]
  (make-router opts))


#_(time
 (let [ent
       {:crux.db/id (java.net.URI. "http://localhost:8000/index.html")
        :crux.web/content-type "text/html;charset=utf-8"
        :crux.web/content-language "en"
        :crux.cms.selmer/template (java.net.URI. "http://localhost:8000/templates/index2.html")}]

   (binding [*custom-resource-path*
             (java.net.URL. "http://localhost:8000/templates/")]
     (selmer/render-file
      (.toURL (:crux.cms.selmer/template ent)) (dissoc ent :template)
      ))))
