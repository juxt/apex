;; Copyright © 2019, JUXT LTD.

;; This ns is deprecated but serves the purpose of showing how to
;; register callback handlers, whose logic should be moved into
;; server.clj in due course.

(ns juxt.apex.examples.petstore.server
  (:require
   [integrant.core :as ig]
   [clojure.java.io :as io]
   ;;[juxt.apex.doc :as doc]
   [clojure.tools.logging :as log]
   ;;[juxt.apex.yaml :as yaml]
   ))

(def database
  (atom {"1" {"name" "Sven" "type" "Dog"}
         "2" {"name" "Luna" "type" "Cat"}
         "3" {"name" "Arya" "type" "Cat"}
         "4" {"name" "Kaia" "type" "Cat"}}))

(defn- nil-doc-exception [document]
  (let [msg (format "No such resource on classpath: %s" document)]
    (ex-info msg {:document document})))

(defmethod ig/init-key ::api
  [_ {:juxt.apex/keys [document]
      :as options}]

  #_(if new-handler-on-each-request?
    ;; Returns a handler that does all the work in reconstructing the
    ;; handler before calling it. Intended for dev mode only.
    (fn [req respond raise]
      ;; TODO: Detect we're in prod mode (by checking existence of a
      ;; dev var) and warn if we're in this code path:
      #_(log/warn "Loading document on request. Performance will be adversely impacted.")

      (log/info "new handler creating")

      (if-let [doc-resource (io/resource document)]
        (let [doc (yaml/parse-string (slurp doc-resource))
              processed-doc (doc/process-document doc)
              h
              (handler
               doc
               (merge
                options
                {:apex/doc doc

                 :apex/operations

                 {"createPets"
                  {:apex/action
                   (fn [req callback raise]
                     (log/info "Create Pets")
                     nil)

                   }

                  "listPets"
                  {:apex/action
                   (fn [req callback raise]
                     (callback
                      (merge
                       req
                       {:apex.response/status 200
                        :apex.response/body
                        (for [[id v] @database]
                          (assoc v "id" id "href" (doc/path-for doc "showPetById" {"petId" id})))})))

                   :apex/validators
                   (fn [req callback raise]
                     {}
                     #_{:apex/entity-tag (hash () body)
                      :apex.validation/strong? true})}

                  "showPetById"
                  {:apex/action
                   (fn [req callback raise]
                     (callback
                      (merge
                       req
                       (let [pet-id (get-in req [:apex.request/path-params "petId"])]
                         (assert pet-id)
                         (if-let [pet (get @database pet-id)]
                           {:apex.response/status 200
                            :apex.response/body
                            (merge
                             {"id" pet-id} ; first in merge order
                             ;; But forbid any id attributes in value
                             (dissoc pet "id"))}

                           ;; TODO: Operations need to have support to
                           ;; provide data bodies that can be rendered
                           ;; with HTML by (another) middleware in dev
                           ;; mode, and stripped of all information in
                           ;; the more secure prod mode.

                           {:apex.response/status 404
                            :apex.response/body (format "Pet not found with id %s" pet-id)})))))

                   }}}))]

          (h req respond raise))
        (raise (nil-doc-exception document))))

    (let [doc (io/resource document)]
      (log/info "Loading document:" document)
      (when-not doc
        (let [e (nil-doc-exception document)]
          (log/fatal e (format "Fatal error loading OpenAPI document: " document))
          (throw e)))

      (handler (yaml/parse-string (slurp doc)) options))))
