;; Copyright © 2019, JUXT LTD.

(ns juxt.apex.alpha2.request
  (:require
   [clojure.tools.logging :as log]
   [juxt.jinx-alpha :as jinx]
   [juxt.apex.alpha2.parameters :as params]
   [juxt.apex.alpha2.conditional-requests :as condreq]
   [juxt.apex.alpha2.head :as head]
   [reitit.ring :as ring]))

;; An attempt to create individual pipelines with a Reitit structure
;; programmatically.

(def add-api-middleware
  {:name "Add API"
   :wrap (fn [h api]
           (fn [req respond raise]
             (h req respond raise)))})

(defn openapi->reitit-routes
  "Create a sequence of Reitit route/resource pairs from a given OpenAPI
  document"
  [doc
   {:apex/keys
    [resources ;; TODO: document this
     handler-middleware-transform
     add-implicit-head? ;; whether HEAD should be supported implicitly
     ]
    :or {add-implicit-head? true}
    :as options}]
  (vec
   (for [[path path-item] (get doc "paths")]
     (let [resource (get resources path)]
       [path
        (apply
         merge
         ;; Support default HEAD method
         (when add-implicit-head? (head/implicit-head-method resource options))
         (for [[method
                {operation-id "operationId"
                 parameters "parameters"}
                operation] path-item
               :let [method (keyword method)]]
           {method
            {:name (keyword operation-id)
             :handler (or (get-in resource [:apex/methods method :handler])
                          (fn [req respond raise]
                            ;; TODO: Create enough data for this
                            ;; to be fancified into HTML by
                            ;; later middleware.
                            (respond
                             {:status 500
                              :body (format "Missing method handler for OpenAPI operation %s at path %s" operation-id path)})))
             :middleware ((or handler-middleware-transform (fn [_ mw] mw))
                          resource
                          [
                           [params/wrap-coerce-parameters parameters]
                           [condreq/wrap-conditional-request (:apex/validators resource)]
                           ])}}))]))))

(defn openapi->reitit-router
  [doc
   {:apex/keys
    [global-middleware-transform ;; function to process a vector of middleware
     ]
    :or {global-middleware-transform identity}
    :as options}]
  (ring/router
   [""
    (openapi->reitit-routes doc options)
    {:data {:middleware
            (global-middleware-transform
             [   ; universal
              [add-api-middleware doc] ; TODO: remove, but it's a good example
              ])}}]))

;; TODO: Rename to openapi/compile-handler or similar
(defn openapi-handler
  "Create a Ring handler from an OpenAPI document, with options"
  [doc
   {:apex/keys
    [default-handler ;; TODO: document this
     ]
    :as options}]
  (ring/ring-handler
   (openapi->reitit-router doc options)
   (or default-handler (ring/create-default-handler))))
