;; Copyright © 2019, JUXT LTD.

(ns juxt.apex.alpha2.parameters
  (:require
   [juxt.jinx-alpha :as jinx]))

(def default-coercions
  {String {"integer" (fn [x] (Integer/parseInt x))}})

(defrecord RequiredParameterMissingError [])

(defrecord ParameterSchemaValidationError [])

(defn process-path-parameters
  ([params paramdefs]
   (process-path-parameters params paramdefs {}))
  ([params paramdefs
    {:keys [coercions]
     :or {coercions default-coercions}}]
   (into
    {}
    (reduce-kv
     (fn [acc pname {:keys [required? schema]}]
       (let [[pkey pval] (find params (keyword pname))]
         (if pkey
           (let [{:keys [valid?] :as validation}
                 (jinx/validate pval schema {:coercions coercions})]
             (assoc
              acc
              pname
              (if valid?
                (:instance validation)
                (map->ParameterSchemaValidationError validation))))

           (if required?
             (assoc acc pname (->RequiredParameterMissingError))
             acc))))
     {}
     paramdefs))))

(defn process-parameters [request paramdefs]
  {:path (process-path-parameters (:path-params request) (:path paramdefs))}
  ;; TODO: do query parameters, header parameters, cookie parameters...
  )

(def wrap-coerce-parameters
  {:name "Extract and coerce parameters"
   :wrap (fn [h parameters]
           (let [paramdefs
                 (into {}
                       (for [[in grp] (group-by #(get % "in") parameters)]
                         [(keyword in)
                          (into {}
                                (for [{pname "name"
                                       required? "required"
                                       desc "description"
                                       schema "schema"} grp]
                                  ;; TODO: If present
                                  [pname {:required? required?
                                          :schema schema}]))]))]
             (fn [req respond raise]
               ;; TODO: Extract parameters out of request, check
               ;; required? and coerce with jinx - see parameters-test
               (h
                (assoc req :apex/parameters (process-parameters req paramdefs))
                respond raise))))})
