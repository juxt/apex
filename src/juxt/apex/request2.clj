;; Copyright © 2019, JUXT LTD.

(ns juxt.apex.request2
  (:require
   [reitit.core :as r]
   [reitit.ring :as ring]
   [clojure.java.io :as io]
   [clojure.tools.logging :as log]
   [juxt.apex.yaml :as yaml]
   [ring.mock.request :as mock]))

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
  [resource]
  {:head
   {:handler (fn [req respond raise] (respond {:status 200}))
    :middleware [
                 [wrap-conditional-request (:apex/validators resource)]
                 ]}})

(defn openapi->reitit-routes [doc {:apex/keys [resources add-implicit-head?]}]
  (vec
   (for [[path path-item] (get doc "paths")]
     (let [resource (get resources path)]
       [path
        (apply
         merge
         ;; Support default HEAD method
         (when add-implicit-head? (head-method resource))
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
             :middleware [
                          [wrap-conditional-request (:apex/validators resource)]
                          ]}}))]))))

(defn openapi->reitit-router [doc options]
  (ring/router
   [""
    {:middleware
     [
      [add-api-middleware doc]
      ]}
    (openapi->reitit-routes doc options)]))

(defn openapi-handler [doc {:apex/keys [resources add-implicit-head?]
                            :or {add-implicit-head? true}
                            :as options}]
  (ring/ring-handler (openapi->reitit-router doc options) (ring/create-default-handler)))
