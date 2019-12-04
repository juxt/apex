;; Copyright © 2019, JUXT LTD.

(ns yada2.alpha.openapi.openapi
  (:require
   [reitit.ring :as ring]))

(defn implicit-head-method
  "Note: the Ring core middleware wrap-head calls into the handler. This
  is an optimized version which does not."
  [resource]
  {:head
   {:handler
    (fn
      ([req] {:status 200})
      ([req respond raise]
       (respond {:status 200})))
    ;; Middleware to be parameterized
    #_:middleware
    #_[
     ;; TODO: Add wrap-coerce-parameters with GET's
     ;; definition
     [condreq/wrap-conditional-request (:apex/validators resource)]
     ]}})

(defn openapi->reitit-routes
  "Create a sequence of Reitit route/resource pairs from a given OpenAPI
  document"
  [doc
   {:apex/keys
    [resources          ;; TODO: document this
     add-implicit-head? ;; whether HEAD should be supported implicitly
     ]
    :or {add-implicit-head? true}
    :as options}]
  (let []

    #_[session-mw
       [session/session-middleware
        {:store (ring.middleware.session.cookie/cookie-store)
         :cookie-name "apex-session"}]]
    (vec
     (for [[path path-item] (get doc "paths")]
       (let [resource (get resources path)]
         [path
          (apply
           merge
           ;; Support default HEAD method
           (when add-implicit-head? (implicit-head-method resource))

           (for [[method {operation-id "operationId"
                          :as operation}]
                 path-item
                 :let [method (keyword method)]]
             {method
              {:name (keyword operation-id)

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

               ;; Middleware to be passed in as a parameter
               ;;:middleware
               #_[
                  ;; TODO: For consistency, copy these naming Reitit
                  ;; conventions in all Apex middleware.
                  ;;[exception/exception-middleware]

                  [condreq/wrap-conditional-request (:apex/validators resource)]
                  params/wrap-coerce-parameters
                  request-body/wrap-process-request-body
                  session-mw
                  ;;trace/wrap-trace
                  ]

               ;; This is needed by some middleware to know whether to
               ;; mount, e.g. a GET method body "has no defined
               ;; semantics".
               :apex/method method

               :apex/operation operation
               :apex/operation-id operation-id
               :apex/path path

               ;; This is needed by some functions as the base document
               ;; in $ref resolution
               :apex/openapi (with-meta doc {:apex.trace/hide true})}}))])))))

(defn create-api-router [doc path {:apex/keys [request-history-atom] :as opts}]
  (ring/router
   (openapi->reitit-routes doc opts)
   (merge
    {:path path}
    #_(when request-history-atom
      {:reitit.middleware/transform (trace/trace-middleware-transform request-history-atom)}))))

;; TODO: Rename from create-api-route to something more intuitive like create-api
(defn create-api-route
  "Create a Reitit route at path that serves an API defined by the given OpenAPI document"
  ([path openapi-doc]
   (create-api-route path openapi-doc {}))
  ([path openapi-doc {:keys [name] :as opts}]
   (let [sub-router (create-api-router openapi-doc path opts)]
     [(str path "*")
      (merge
       (when name {:name name})
       {:handler
        (let [sub-handler
              (fn [req]
                (let [path (get-in req [:reitit.core/match :path])]
                  (get-in req [:reitit.core/match :data :sub-handler])))]
          (fn
            ([req] ((sub-handler req) req))
            ([req respond raise] ((sub-handler req) req respond raise))))
        :sub-handler
        (ring/ring-handler
         sub-router
         nil
         {:middleware
          [#_(fn [h]
               (fn
                 ([req] (h (assoc req :apex/router sub-router)))
                 ([req respond raise] (h (assoc req :apex/router sub-router) respond raise))))]})})])))


;; Deprecated - openapi-test still uses this but needs to be
;; refactored to use create-api-route above.

#_(defn openapi->reitit-router
  [doc {:keys [reitit.middleware/transform] :as options}]
  (ring/router
   [""
    (openapi->reitit-routes doc options)
    {:data
     {:middleware
      [[response/server-header-middleware "JUXT Apex"]]}}]
   (select-keys options [:reitit.middleware/transform])))

#_(defn compile-handler
  "Create a Ring handler from an OpenAPI document, with options"
  [doc
   {:keys
    [default-handler
     reitit.middleware/transform]
    :as options}]
  (ring/ring-handler
   (openapi->reitit-router doc options)
   (or
    default-handler
    (ring/create-default-handler
     {:not-found (fn [req]
                   {:status 404
                    :body "Apex: Not found"})}))))
