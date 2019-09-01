(ns juxt.apex.dev.api
  (:require
   [integrant.core :as ig]
   [clojure.java.io :as io]
   [juxt.apex.request :refer [handler]]
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
        (let [h (handler
                 (yaml/parse-string (slurp doc))
                 (merge
                  options
                  {:operation-handlers
                   {"createPets" (fn [_]
                                   (log/info "Create Pets")
                                   nil)
                    "listPets" (fn [_ _ _]
                                 ["cat" "dog"])}}))]
          (h req respond raise))
        (raise (nil-doc-exception document))))

    (let [doc (io/resource document)]
      (log/info "Loading document:" document)
      (when-not doc
        (let [e (nil-doc-exception document)]
          (log/fatal e (format "Fatal error loading OpenAPI document: " document))
          (throw e)))

      (handler (yaml/parse-string (slurp doc)) options))))
