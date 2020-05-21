(ns juxt.apex.alpha.cms.core
  (:require
   [juxt.apex.alpha.async.helpers :as a]
   [clojure.pprint :refer [pprint]]
   [ring.middleware.params :refer [wrap-params]]
   [ring.middleware.head :refer [wrap-head]]
   [clojure.java.io :as io]
   [selmer.parser :as selmer]
   [selmer.util :refer [*custom-resource-path*]]
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
        (str/replace "<" "&lt;"))
       "</pre>"))

(defn respond-entity [ent req respond raise]
  (respond
   {:status 200
    :headers {"content-type" "text/html"}
    :body (entity-as-html ent)}))

(def templates-source-uri (java.net.URI. "http://localhost:8000/_sources/templates/"))

(defn render-entity-with-selmer-template [ent]
  (binding [*custom-resource-path*
            (. templates-source-uri toURL)]
    (selmer/render-file
     (java.net.URL. (str templates-source-uri (:crux.cms.selmer/template ent))) (dissoc ent :template)
     :custom-resource-path (. templates-source-uri toURL))))

(defn redirect? [ent]
  (when-let [status (:crux.web/status ent)]
    (and (>= status 300) (< status 400))))

(defn rfc1123-date [inst]
  (.
   (java.time.format.DateTimeFormatter/RFC_1123_DATE_TIME)
   format
   inst))

(defmulti method (fn [req respond raise opts] (:request-method req)))

(defmethod method :default [req respond raise opts]
  (respond
   {:status 501}))

(defmethod method :options [req respond raise opts]
  ;; TODO: Check path?
  (respond
   {:status 200
    :headers {"DAV" "1,2"}}))


(defn respond-entity-response [ent vertx req respond raise]
  (try
    (cond
      (redirect? ent)
      (respond
       {:status (:crux.web/status ent)
        :headers {"location" (str (:crux.web/location ent))}})

      (:crux.cms/file ent)
      (respond
       {:status 200
        :body
        (io/file (:crux.cms/file ent))})

      (string? (:crux.cms/content ent))
      (respond
       {:status 200
        :headers
        (cond-> {}
          (:crux.web/content-type ent)
          (conj ["content-type" (:crux.web/content-type ent)])
          (:crux.web/content-language ent)
          ;; TODO: Support vectors for multiple languages
          (conj ["content-language" (:crux.web/content-language ent)]))
        :body (:crux.cms/content ent)})

      (:crux.cms.selmer/template ent)
      (a/execute-blocking-code
       vertx
       (fn [] (render-entity-with-selmer-template ent))
       {:on-success
        (fn [body]
          (respond
           {:status 200
            :headers
            (cond-> {}
              (:crux.web/content-type ent)
              (conj ["content-type" (:crux.web/content-type ent)])
              (:crux.web/content-language ent)
              ;; TODO: Support vectors for multiple languages (see
              ;; identical TODO above)
              (conj ["content-language" (:crux.web/content-language ent)]))

            :body body}))
        :on-failure
        (fn [t]
          (raise
           (ex-info
            "Failed to render template"
            {:template (:crux.cms.selmer/template ent)} t)))})

      :else
      (respond
       {:status 500
        :body
        (str
         "<body><h2>ERROR - Not handled</h2>"
         (entity-as-html ent)
         "</body>")}))

    (catch Throwable t
      (raise (ex-info (format "Error with path: %s" (:uri req)) {:request req} t))
      )))

(defn url-rewrite-request [request {:keys [canonical _]}]
  (-> request
      (assoc-in [:headers "host"] (:host-header canonical))
      (assoc :scheme (:scheme canonical))))

(defn url-rewrite-response [response _]
  response)

(defn wrap-url-rewrite [handler opts]
  (fn
    ([request]
     (-> request
         (url-rewrite-request opts)
         handler
         (url-rewrite-response opts)))
    ([request respond raise]
     (handler
      (url-rewrite-request request opts)
      (fn [response] (respond (url-rewrite-response response opts)))
      raise))))

(defn wrap-log [handler]
  (fn
    ([request]
     (println "Incoming sync CMS request:\n" (with-out-str (pprint request)))
     (let [response (handler request)]
       (println "Outgoing CMS response:" (with-out-str (pprint response)))
       response))
    ([request respond raise]
     (println "Incoming async CMS request:\n" (with-out-str (pprint request)))
     (handler
      request
      (fn [response]
        (println "Outgoing CMS response:" (with-out-str (pprint response)))
        (respond response))
      (fn [t]
        (println "Error raised:" t)
        (raise t))))))

(defmethod method :get [req respond raise {:keys [vertx store]}]
  (let [debug (get-in req [:query-params "debug"])]

    (if-let [ent (find-entity
                  store
                  (java.net.URI. (uri req)))]
      (if debug
        (respond-entity ent req respond raise)
        (respond-entity-response ent vertx req respond raise))

      (respond {:status 404 :body "Crux CMS: 404 (Not found)\n"}))))

(defn make-router [{:keys [store vertx] :as opts}]
  (assert store)
  (assert vertx)
  (->
   (fn this
     ([req]
      (this req identity (fn [t] (throw t))))
     ([req respond raise]
      (method req respond raise opts)))

   ;; To get the debug query parameter.  Arguably we could use Apex's
   ;; OpenAPI-compatible replacement.
   wrap-params

   ;; This is for strict semantics, but handlers should still check
   ;; the request-method prior to generating expensive bodies.
   wrap-head

   ;; Dev only, removed on production
   (wrap-url-rewrite
    {:canonical {:scheme :https :host-header "juxt.pro"}
     :actual {:scheme :http :host-header "localhost:8000"}})

   wrap-log

   a/wrap-request-body-as-input-stream
   ))
