;; Copyright © 2019, JUXT LTD.

;; Not used

(ns juxt.apex.alpha2.coercion
  (:require
   [reitit.coercion :as coercion]
   [juxt.jinx-alpha :as jinx]
   [clojure.tools.logging :as log]))

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

(def coercion (create {}))
