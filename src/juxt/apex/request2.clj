;; Copyright © 2019, JUXT LTD.

(ns juxt.apex.request2
  (:require
   [juxt.apex.coercion :as coercion]
   [reitit.ring :as ring]
   [reitit.ring.coercion :as rrc]))

;; An attempt to create individual pipelines with a Reitit structure
;; programmatically.

(def add-api-middleware
  {:name "Add API"
   :wrap (fn [h api]
           (fn [req respond raise]
             (h req respond raise)))})

;; -- BEGIN: Conditional Requests --

(defmulti validator->header (fn [[k v]] k))

(defmethod validator->header :apex/entity-tag
  [[_ v]] ["ETag" (:value v)])

(defn to-rfc-1123-date-time [instant]
  (.format java.time.format.DateTimeFormatter/RFC_1123_DATE_TIME
           (java.time.ZonedDateTime/ofInstant instant (java.time.ZoneId/of "GMT"))))

(defn from-rfc-1123-date-time [s]
  (some->> s
           (.parse java.time.format.DateTimeFormatter/RFC_1123_DATE_TIME)
           (java.time.Instant/from)))

(defmethod validator->header :apex/last-modified
  [[_ v]]
  ["Last-Modified" (to-rfc-1123-date-time (:value v))])

(defn since? [instant since]
  (= (.compareTo instant since) 1))

(defn wrap-conditional-request [h validators]
  (fn [req respond raise]
    (let [operation (:oas/operation req)
          opId (get operation "operationId")]
      (if validators
        (validators
         req
         (fn [req]
           (let [since (from-rfc-1123-date-time (get-in req [:headers "if-modified-since"]))
                 modified (:value (:apex/last-modified req))]

             (if (and since modified (not (since? modified since)))
               (respond {:status 304})
               (h req
                  (fn [response]
                    (respond (update response :headers merge (into {} (map validator->header (select-keys req [:apex/entity-tag :apex/last-modified]))))))
                  raise))))
         raise)
        ;; No validators
        (h req respond raise)))))

;; -- END: Conditional Requests --

(defn head-method
  "Note: the Ring core middleware wrap-head calls into the handler. This
  is an optimized version which does not."
  [resource {:apex/keys [handler-middleware-transform]}]
  {:head
   {:handler (fn [req respond raise] (respond {:status 200}))
    :middleware ((or handler-middleware-transform (fn [_ mw] mw))
                 resource
                 [rrc/coerce-request-middleware
                  [wrap-conditional-request (:apex/validators resource)]
                  ])}})

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
         (when add-implicit-head? (head-method resource options))
         (for [[method operation] path-item
               :let [method (keyword method)
                     operation-id (get operation "operationId")]]
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
             :coercion coercion/coercion
             :middleware ((or handler-middleware-transform (fn [_ mw] mw))
                          resource
                          [rrc/coerce-request-middleware
                           [wrap-conditional-request (:apex/validators resource)]
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
             [
              [add-api-middleware doc]  ; universal
              ])}}]))

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
