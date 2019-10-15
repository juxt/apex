;; Copyright © 2019, JUXT LTD.

(ns juxt.apex.alpha2.parameters
  (:require
   [clojure.string :as str]
   [juxt.apex.alpha2.errors :refer [if-error]]
   [juxt.jinx-alpha :as jinx]
   muuntaja.format.core
   muuntaja.format.json
   [muuntaja.core :as muuntaja]
   [ring.util.codec :refer [url-decode]]))

(defmulti format-with-style
  (fn [n value {:keys [style explode?]}]
    {:style style
     :explode? explode?
     :type (cond (nil? value) :empty
                 (string? value) :string
                 (sequential? value) :array
                 (map value) :object)}))

(defmethod format-with-style {:style "matrix" :explode? true :type :empty}
  [n value _]
  (str \; n))

(defmethod format-with-style {:style "matrix" :explode? true :type :string}
  [n value _]
  (format ";%s=%s" n value))

(defmethod format-with-style {:style "matrix" :explode? true :type :array}
  [n value _]
  (str/join (map #(format ";%s=%s" %1 %2) (repeat n) value)))

(defmethod format-with-style {:style "matrix" :explode? true :type :object}
  [n value _]
  (str/join (for [[k v] value] (str \; k \= v))))

(defmethod format-with-style {:style "matrix" :explode? false :type :empty}
  [n value _]
  (str \; n))

(defmethod format-with-style {:style "matrix" :explode? false :type :string}
  [n value _]
  (format ";%s=%s" n value))

(defmethod format-with-style {:style "matrix" :explode? false :type :array}
  [n value _]
  (format ";%s=%s" n (str/join \, value)))

(defmethod format-with-style {:style "matrix" :explode? false :type :object}
  [n value _]
  (format ";%s=%s" n (str/join "," (mapcat seq value))))

(defmethod format-with-style {:style "label" :explode? false :type :empty}
  [n value _]
  ".")

(defmethod format-with-style {:style "label" :explode? false :type :string}
  [n value _]
  (str \. value))

(defmethod format-with-style {:style "label" :explode? false :type :array}
  [n value _]
  (str \. (str/join \. value)))

(defmethod format-with-style {:style "label" :explode? false :type :object}
  [n value _]
  (str \. (str/join \. (mapcat seq value))))

(defmethod format-with-style {:style "label" :explode? true :type :empty}
  [n value _]
  ".")

(defmethod format-with-style {:style "label" :explode? true :type :string}
  [n value _]
  (str \. value))

(defmethod format-with-style {:style "label" :explode? true :type :array}
  [n value _]
  (str \. (str/join \. value)))

(defmethod format-with-style {:style "label" :explode? true :type :object}
  [n value _]
  (str \. (str/join \. (map (fn [[k v]] (str k \= v)) value))))

(defmethod format-with-style {:style "form" :explode? false :type :empty}
  [n value _]
  (str n \=))

(defmethod format-with-style {:style "form" :explode? false :type :string}
  [n value _]
  (str n \= value))

(defmethod format-with-style {:style "form" :explode? false :type :array}
  [n value _]
  (str n \= (str/join \, value)))

(defmethod format-with-style {:style "form" :explode? false :type :object}
  [n value _]
  (str n \= (str/join \, (mapcat seq value))))

(defmethod format-with-style {:style "form" :explode? true :type :empty}
  [n value _]
  (str n \=))

(defmethod format-with-style {:style "form" :explode? true :type :string}
  [n value _]
  (str n \= value))

(defmethod format-with-style {:style "form" :explode? true :type :array}
  [n value _]
  (str/join \& (for [v value] (str n \= v))))

(defmethod format-with-style {:style "form" :explode? true :type :object}
  [n value _]
  (str/join \& (for [[k v] value] (str k \= v))))

(defmethod format-with-style {:style "simple" :explode? false :type :string}
  [n value _]
  value)

(defmethod format-with-style {:style "simple" :explode? false :type :array}
  [n value _]
  (str/join \, value))

(defmethod format-with-style {:style "simple" :explode? false :type :object}
  [n value _]
  (str/join \, (mapcat seq value)))

(defmethod format-with-style {:style "simple" :explode? true :type :string}
  [n value _]
  value)

(defmethod format-with-style {:style "simple" :explode? true :type :array}
  [n value _]
  (str/join \, value))

(defmethod format-with-style {:style "simple" :explode? true :type :object}
  [n value _]
  (str/join \, (map (fn [[k v]] (str k \= v)) value)))

(defmethod format-with-style {:style "spaceDelimited" :explode? false :type :array}
  [n value _]
  (str/join "%20" value))

(defmethod format-with-style {:style "spaceDelimited" :explode? false :type :object}
  [n value _]
  (str/join "%20" (mapcat seq value)))

(defmethod format-with-style {:style "pipeDelimited" :explode? false :type :array}
  [n value _]
  (str/join \| value))

(defmethod format-with-style {:style "pipeDelimited" :explode? false :type :object}
  [n value _]
  (str/join \| (mapcat seq value)))

(defmethod format-with-style {:style "deepObject" :explode? true :type :object}
  [n value _]
  (str/join \& (for [[k v] value] (str n "[" k "]=" v))))

;; ---

(def default-coercions
  {String {"integer" (fn [x] (Integer/parseInt x))
           "number" (fn [x] (Double/parseDouble x))
           "array" vector
           "boolean" (fn [x] (case x "true" true "false" false nil))}
   nil {"string" (constantly "")
        ;;"boolean" (constantly false)
        }})

(defn- group-query-string [qs]
  (reduce-kv
   (fn [acc k v] (assoc acc k (mapv second v)))
   {}
   (->> (str/split qs #"&")
        (filter (comp not str/blank?) )
        (map #(str/split % #"=") )
        (group-by first))))

(defn- seq->map [vs]
  (loop [[k v & vs] vs result {}]
    (if v
      (recur vs (assoc result k v))
      result)))

(defn- extract-parameter-for-object
  "For a given parameter with name n, of type object, with OpenAPI
  parameter declaration in param, extract the parameter from the
  query-state map in :apex/qsm. If a parameter can be extracted, conj
  to :apex/params in acc. If an error occurs, conj the error
  to :apex/errors in acc. The reason for this contrived function
  signature is to support integration with a reduction across
  parameter declarations for a given query-string."
  [{:apex/keys [qsm params errors] :as acc} [n param]]
  (let [{:strs [schema required style explode]} param
        {:keys [value raw-values]
         new-qsm :qsm}
        (reduce-kv
         (fn [acc prop-key {typ "type" :as schema}]
           (let [qsm-key (if (and explode (= style "deepObject"))
                           (str n "[" prop-key "]")
                           prop-key)]
             (if-let [[_ encoded-values] (find qsm qsm-key)]
               (let [val
                     (cond
                       ;; If the requires a boolean value, we treat an
                       ;; 'empty' value as a true.
                       (and (= typ "boolean") (nil? (first encoded-values)))
                       true
                       :else (first encoded-values))]
                 (-> acc
                     (update :value assoc prop-key val)
                     (update :qsm dissoc qsm-key)
                     (update :raw-values conj [prop-key encoded-values])))
               acc)))
         {:qsm qsm
          :value {}
          :raw-values []}
         (get schema "properties"))

        validation
        (jinx/validate value schema {:coercions default-coercions})]

    (cond-> acc
      ;; set qsm to new-qsm
      true (assoc :apex/qsm new-qsm)
      ;; If valid, add the parameter
      (:valid? validation)
      (update :apex/params conj [n {:raw-values raw-values
                                    :value (:instance validation)
                                    :param param}])
      ;; If not valid, add an error
      ;; TODO: Adding errors should be a common function to promote
      ;; consistency across errors.
      (not (:valid? validation))
      (update :apex/errors conj {:param [n param] :message "Not valid" :validation validation}))))

(defn extract-undeclared-params [{:apex/keys [params qsm] :as state}]
  (-> (reduce-kv
       (fn [state k v]
         (update state :apex/params conj [k {:value v}])
         )
       state
       qsm)
      (dissoc :apex/qsm)))

(defn- extract-value-from-encoded-values+
  "Extract a value for the given parameter from a collection
  of (possibly) encoded values. Returns value according to parameter
  schema, or an error."
  [{:strs [style explode required schema content]
    allow-empty-value "allowEmptyValue"
    :or {style "form"}
    :as param}
   encoded-values
   muuntaja]

  (if content
    ;; Complex Situation
    (let [[media-type {:strs [schema]}]
          ;; "The map MUST only contain one entry."
          (first content)]
      (muuntaja/decode muuntaja media-type (first encoded-values)))

    ;; Simple Situation
    (case [(get schema "type" "string") explode]

      (["null" true]
       ["null" false])
      ;; TODO: Think about how to represent nils in query params - is there guidance here?
      ;; An empty value is an implicit nil, but how to specify nil when allowEmptyValue is false ?
      (if allow-empty-value
        (first encoded-values)
        {:apex/error {:apex.error/message "Empty value not allowed for parameter: allowEmptyValue is false"
                      :apex.error/references [{:apex.error.reference/url ""}]}})

      (["boolean" false]
       ["boolean" true])
      (if-some [val (first encoded-values)]
        (if (or (.equalsIgnoreCase "false" val)
                (.equalsIgnoreCase "no" val)
                (.equalsIgnoreCase "nil" val))
          false true)
        (if allow-empty-value
          true
          {:apex/error {:apex.error/message "Empty value not allowed for parameter: allowEmptyValue is false"}}))

      (["integer" false]          ; single param in collection, take it
       ["integer" true]) ; possibly multiple params in collection, take first
      (if-some [val (first encoded-values)]
        (try
          (Integer/parseInt val)
          (catch Exception e
            {:apex/error {:apex.error/message "Failed to coerce value to integer"
                          :value val
                          :apex.error/exception e}}))
        (if allow-empty-value
          nil
          {:apex/error {:apex.error/message "Empty value not allowed for parameter: allowEmptyValue is false"}}))

      (["string" false]          ; single param in collection, take it
       ["string" true]) ; possibly multiple params in collection, take first
      (if-some [val (first encoded-values)]
        val
        (if allow-empty-value
          nil
          {:apex/error {:apex.error/message "Empty value not allowed for parameter: allowEmptyValue is false"}}))

      ["array" false]
      (str/split
       (first encoded-values)
       (case style "form" #"," "spaceDelimited" #" " "pipeDelimited" #"\|"))

      ["array" true]
      (if (or
           allow-empty-value
           (every? some? encoded-values))
        (vec encoded-values)
        {:apex/error {:apex.error/message "Empty values not allowed for parameter: allowEmptyValue is false"}})

      ["object" false]
      (seq->map
       (str/split
        (first encoded-values)
        (case style "form" #"," "spaceDelimited" #" " "pipeDelimited" #"\|")))

      ["object" true]
      (->> encoded-values
           (map #(str/split % (case style "form" #"," "spaceDelimited" #" " "pipeDelimited" #"\|")))
           (into {})))))

(def default-muuntaja
  (muuntaja/create
   (->
    muuntaja/default-options
    (assoc-in [:formats "application/json"]
              (muuntaja.format.core/map->Format
               {:name "application/json"
                :decoder [muuntaja.format.json/decoder {:decode-key-fn false}]
                :encoder [muuntaja.format.json/encoder]})))))

;; TODO: Rename to process-query-string
(defn parse-query-string
  ([qs paramdefs]
   (parse-query-string qs paramdefs {}))
  ([qs paramdefs {:keys [muuntaja]
                  :or {muuntaja default-muuntaja}}]
   (let [qsm (group-query-string ((fnil url-decode "") qs))]
     (->
      (reduce
       (fn [{:apex/keys [params qsm errors] :as acc}
            {:strs [in style explode schema required]
             n "name"
             allow-empty-value "allowEmptyValue"
             :or {style "form"}
             :as param}]
         ;; When style is form, the default value is true. For all other
         ;; styles, the default value is false.
         (case in
           "query"
           (let [explode (if (some? explode) explode (= style "form"))]
             (if (and (.equals "object" (get schema "type"))
                      (or (.equals "form" style) (.equals "deepObject" style))
                      explode)

               (extract-parameter-for-object acc [n param])

               (if-let [[_ encoded-values] (find qsm n)]

                 (let [acc (update acc :apex/qsm dissoc n)
                       value+        ; + postfix indicates maybe error
                       (extract-value-from-encoded-values+
                        (assoc param "explode" explode)
                        encoded-values
                        muuntaja)]

                   (if-error value+
                     (update acc :apex/errors conj (merge error {:apex/param [n param]}))

                     (let [validation (jinx/validate value schema {:coercions default-coercions})]

                       (cond-> acc
                         (:valid? validation)
                         (update :apex/params conj [n {:raw-values encoded-values
                                                       :value (:instance validation)
                                                       :param param
                                                       :validation validation}])

                         (not (:valid? validation))
                         ;; TODO: Errors should follow the same convention, using consistent keyword namespaces
                         (update :apex/errors conj {:apex/param [n param] :message "Not valid" :validation validation})

                         true (update :apex/qsm dissoc n)))))

                 ;; No value exists
                 (cond-> acc
                   ;; We're going to add an entry anyway. One purpose
                   ;; is to show tables in debug mode.
                   true (update :apex/params conj [n {:param param}])
                   ;; TODO: Errors should follow the same convention, using consistent keyword namespaces
                   required (update :apex/errors conj {:param n :message "Required parameter missing"}) ))))

           ;; Default is to pass acc long to the next parameter
           acc

           ))

       {:apex/params [] :apex/qsm qsm :apex/errors []}
       ;; Reduce over paramdefs
       paramdefs)

      extract-undeclared-params
      (update :apex/params #(into {} %))))))

;; TODO: Consider replacing these records with thrown exceptions and
;; catching. There is additional complexity to detecting and handling
;; these errors. Exceptions on the JVM are cheap.

;; The code below was written prior to the query param parser and
;; follows a different design/style.

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
    (reduce-kv ; reduce over paramdefs
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
  {;;:path (process-path-parameters (:path-params request) (:path paramdefs))
   ;;:query (parse-query-string (:query paramdefs))
   }

  ;; TODO: do query parameters
  ;; TODO: do header parameters
  ;; TODO: do cookie parameters
  )

(def wrap-coerce-parameters
  {:name "Extract and coerce parameters"

   :compile
   (fn [{:apex/keys [operation]} opts]
     (let [{:strs [parameters]} operation
           query-parameters (not-empty (filter #(= (get % "in") "query") parameters))
           process-req
           (fn [req]
             (assoc
              req
              :apex/parameters
              (cond-> {}
                query-parameters
                (assoc :query (parse-query-string (:query-string req) parameters)))))]

       (fn [h _]
         (fn
           ([req] (h (process-req req)))
           ([req respond raise] (h (process-req req) respond raise))))))

   ;; TODO: Add :compile which should fail if there is a declared path
   ;; parameter but none in the URI

   })
