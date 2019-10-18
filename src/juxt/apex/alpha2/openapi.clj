(ns juxt.apex.alpha2.openapi
  (:require
   [clojure.tools.logging :as log]
   [juxt.jinx-alpha :as jinx]
   [juxt.apex.alpha2.trace :as trace]
   [juxt.apex.alpha2.parameters :as params]
   [juxt.apex.alpha2.request-body :as request-body]
   [juxt.apex.alpha2.response :as response]
   [juxt.apex.alpha2.conditional-requests :as condreq]
   [juxt.apex.alpha2.head :as head]
   [reitit.ring :as ring]))

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
         (for [[method {operation-id "operationId"
                        :as operation}]
               path-item
               :let [method (keyword method)]]
           {method
            {
             :name (keyword operation-id)

             :handler
             (let [default-response
                   {:status 500
                    ;; TODO: Create enough data for this to
                    ;; be fancified into HTML by later
                    ;; middleware.
                    :body (format "Missing method handler for OpenAPI operation %s at path %s\n" operation-id path)}]
               (or

                (get-in resource [:apex/methods method :handler])

                (fn
                  ([req] default-response)
                  ([req respond raise]
                   (respond default-response)))))

             :middleware
             ( ;; don't need this because reitit has :transform
              (or handler-middleware-transform (fn [_ mw] mw))
              resource
              [[condreq/wrap-conditional-request (:apex/validators resource)]
               params/wrap-coerce-parameters
               request-body/wrap-process-request-body
               trace/wrap-trace])

             ;; This is needed by some middleware to know whether to
             ;; mount, e.g. a GET method body "has no defined
             ;; semantics".
             :apex/method method

             :apex/operation operation
             :apex/operation-id operation-id
             :apex/path path

             ;; This is needed by some functions as the base document
             ;; in $ref resolution
             :apex/openapi (with-meta doc {:apex.trace/hide true})}}))]))))

(defn openapi->reitit-router
  [doc options]
  (ring/router
   [""
    (openapi->reitit-routes doc options)
    {:data {:middleware
            [[response/server-header-middleware "JUXT Apex"]

             ]}}]))

(defn compile-handler
  "Create a Ring handler from an OpenAPI document, with options"
  [doc
   {:apex/keys
    [default-handler ;; TODO: document this
     ]
    :as options}]
  (ring/ring-handler
   (openapi->reitit-router doc options)
   (or
    default-handler
    (ring/create-default-handler
     {:not-found (fn [req]
                   {:status 404
                    :body "Apex: Not found"})}))))
