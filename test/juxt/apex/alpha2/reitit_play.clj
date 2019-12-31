;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.alpha2.reitit-play
  (:require
   [reitit.coercion :as coercion]
   [reitit.ring.coercion :as rrc]
   [reitit.core :as r]
   [reitit.ring :as ring]
   [juxt.jinx-alpha :as jinx]
   [clojure.tools.logging :as log]
   [reitit.coercion.schema]
   [schema.core :as s]))

(defn create [opts]
  (reify coercion/Coercion
    (-get-name [_] :jinx)
    (-get-options [_] opts)
    (-compile-model [_ model _] model)
    (-open-model [_ schema]
      schema)
    (-encode-error [_ error]
      ;; TODO: These pr-str are just to make the code compile, see
      ;; reitit.coercion.schema for hints on what this implementation
      ;; should be.
      (-> error
          (update :schema pr-str)
          (update :errors pr-str)))
    (-request-coercer [_ type schema]
      (fn [value format]
        (let [[post-validation-value errors]
              (reduce-kv
               (fn [[value errors] k schema]
                 (let [validation
                       (jinx/validate
                        (get value k)
                        schema
                        {:coercions
                         {String
                          {"integer" (fn [x] (Integer/parseInt x))}}})]

                   (if (:valid? validation)
                     [(assoc value k (:instance validation)) errors]
                     [value (assoc errors k
                                   (coercion/map->CoercionError
                                    {:schema schema
                                     :errors (:errors validation)})) ])))
               ;; A pair of value and errors
               [value {}]
               schema)]

          (log/trace "post-validation-value" (pr-str post-validation-value))

          (if (empty? errors)
            post-validation-value
            (coercion/map->CoercionError
             {:schema schema
              :errors errors})))))

    (-response-coercer [this schema]
      (log/error "response-coercer")
      (throw (ex-info "response-coercer" {}))
      #_(if (coerce-response? schema)
          (coercion/-request-coercer this :response schema)))))

(def coercion
  (create
   {}))

(def router
  (ring/router
   ["/{company}/users/{user-id}"
    {:name ::user-view
     :get {:handler (fn [req] {:status 200 :body (get req :parameters)})
           :coercion coercion
           :parameters {:path {:company {"type" "string"}
                               :user-id {"type" "integer"}}}}}]
   {:data {:middleware [rrc/coerce-request-middleware]}}))

(def app
  (ring/ring-handler
   router
   (fn [_] {:status 500 :body "default handler applied"})))

(app {:request-method :get
      :uri "/juxt/users/123"})

;; TODO: Make this work with coercions!


;; TODO: Convert OpenAPI to JSON Schema prior to calling this to
;; ensure int32 gets turned to integer
