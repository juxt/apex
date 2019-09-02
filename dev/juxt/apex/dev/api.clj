(ns juxt.apex.dev.api
  (:require
   [integrant.core :as ig]
   [clojure.java.io :as io]
   [juxt.apex.request :refer [handler]]
   [juxt.apex.doc :as doc]
   [clojure.tools.logging :as log]
   [juxt.apex.yaml :as yaml]))

(defn- nil-doc-exception [document]
  (let [msg (format "No such resource on classpath: %s" document)]
    (ex-info msg {:document document})))

(defmethod ig/init-key ::api
  [_ {:juxt.apex/keys [document]
      :juxt.apex.dev/keys [new-handler-on-each-request?]
      :as options}]

  (if new-handler-on-each-request?
    ;; Returns a handler that does all the work in reconstructing the
    ;; handler before calling it. Intended for dev mode only.
    (fn [req respond raise]
      ;; TODO: Detect we're in prod mode (by checking existence of a
      ;; dev var) and warn if we're in this code path:
      #_(log/warn "Loading document on request. Performance will be adversely impacted.")

      (if-let [doc (io/resource document)]
        (let [doc (doc/process-document (yaml/parse-string (slurp doc)))
              h
              (handler
               doc
               (merge
                options
                {:apex/operations

                 {"createPets"
                  {:apex/action
                   (fn [req callback raise]
                     (log/info "Create Pets")
                     nil)

                   :apex/validators
                   []}

                  "listPets"
                  {:apex/action
                   (fn [req callback raise]
                     (callback
                      (merge
                       req
                       {:apex.response/status 200
                        :apex.response/body
                        [{:pet "cat" :href (doc/path-for doc "showPetById" {"petId" 1})}
                         {:pet "dog" :href (doc/path-for doc "showPetById" {"petId" 2})}]})))

                   :apex/validators
                   [(fn [body]
                      {:apex/entity-tag (hash body)
                       :apex.validation/strong? true})]}


                  "showPetById"
                  {:apex/action
                   (fn [req callback raise]
                     (callback
                      (merge
                       req
                       {:apex.response/status 200
                        :apex.response/body
                        ;; TODO: Look up pet details in a database

                        ;; TODO: Key goal is to generate an ETag and
                        ;; support If-Match to avoid the lost-update
                        ;; problem with PUT.

                        {"name" "Rover"}})))

                   :apex/validators
                   []}

                  }}))]

          (h req respond raise))
        (raise (nil-doc-exception document))))

    (let [doc (io/resource document)]
      (log/info "Loading document:" document)
      (when-not doc
        (let [e (nil-doc-exception document)]
          (log/fatal e (format "Fatal error loading OpenAPI document: " document))
          (throw e)))

      (handler (yaml/parse-string (slurp doc)) options))))
