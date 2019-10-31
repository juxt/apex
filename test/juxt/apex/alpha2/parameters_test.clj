;; Copyright © 2019, JUXT LTD.

(ns juxt.apex.alpha2.parameters-test
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is are testing]]
   [juxt.apex.alpha2.parameters
    :refer [process-path-parameters
            format-with-style
            process-query-string]]
   [criterium.core :as crt]
   muuntaja.format.core
   muuntaja.format.json
   [muuntaja.core :as m]
   [ring.util.codec :as codec])
  (:import
   (juxt.apex.alpha2.parameters
    RequiredParameterMissingError
    ParameterSchemaValidationError)))

;; Formatting

;; Taken from https://swagger.io/specification/#parameterObject
(def style-test-data
  [{:style "matrix",
    :explode "false",
    :empty ";color",
    :string ";color=blue",
    :array ";color=blue,black,brown",
    :object ";color=R,100,G,200,B,150"}
   {:style "matrix",
    :explode "true",
    :empty ";color",
    :string ";color=blue",
    :array ";color=blue;color=black;color=brown",
    :object ";R=100;G=200;B=150"}
   {:style "label",
    :explode "false",
    :empty ".",
    :string ".blue",
    :array ".blue.black.brown",
    :object ".R.100.G.200.B.150"}
   {:style "label",
    :explode "true",
    :empty ".",
    :string ".blue",
    :array ".blue.black.brown",
    :object ".R=100.G=200.B=150"}
   {:style "form",
    :explode "false",
    :empty "color=",
    :string "color=blue",
    :array "color=blue,black,brown",
    :object "color=R,100,G,200,B,150"}
   {:style "form",
    :explode "true",
    :empty "color=",
    :string "color=blue",
    :array "color=blue&color=black&color=brown",
    :object "R=100&G=200&B=150"}
   {:style "simple",
    :explode "false",
    :empty "n/a",
    :string "blue",
    :array "blue,black,brown",
    :object "R,100,G,200,B,150"}
   {:style "simple",
    :explode "true",
    :empty "n/a",
    :string "blue",
    :array "blue,black,brown",
    :object "R=100,G=200,B=150"}
   {:style "spaceDelimited",
    :explode "false",
    :empty "n/a",
    :string "n/a",
    :array "blue%20black%20brown",
    :object "R%20100%20G%20200%20B%20150"}
   {:style "pipeDelimited",
    :explode "false",
    :empty "n/a",
    :string "n/a",
    :array "blue|black|brown",
    :object "R|100|G|200|B|150"}
   {:style "deepObject",
    :explode "true",
    :empty "n/a",
    :string "n/a",
    :array "n/a",
    :object "color[R]=100&color[G]=200&color[B]=150"}])

;; Test all the cases found in style-test-data
(deftest style-test
  (doseq [m style-test-data
          [k v] {:empty nil
                 :string "blue"
                 :array ["blue","black","brown"]
                 :object {"R" 100 "G" 200 "B" 150}}
          :let [expected (get m k)
                style (get m :style)
                explode? (Boolean/valueOf (get m :explode))]
          :when (not= expected "n/a")]

    (testing (format "Format '%s' with style '%s', explode %s, type %s" v style explode? k)
      (is (= expected (format-with-style "color" v {:style style :explode? explode?}))))))

;; Simple Scenarios

;; "The rules for serialization of the parameter are specified in one
;; of two ways. For simpler scenarios, a schema and style can describe
;; the structure and syntax of the parameter."



;; TODO: Test performance with criterium
;; TODO: Attempt to improve performance

#_(process-query-string
           "a"
           [{"name" "a"
             "in" "query"
             "style" "form"
             "allowEmptyValue" false
             "schema" {"type" "null"}}])

;; https://stackoverflow.com/questions/4557387/is-a-url-query-parameter-valid-if-it-has-no-value
(deftest empty-value-test
  (testing "allowedEmptyValue is true"
    (let [result (process-query-string
                  "a"
                  [{"name" "a"
                    "in" "query"
                    "style" "form"
                    "allowEmptyValue" true
                    "schema" {"type" "null"}}])]
      (let [[n v] (find result "a")]
        (is (= "a" n))
        (is (nil? (:value v))))))

  (testing "allowedEmptyValue is false"
    (let [result
          (process-query-string
           "a"
           [{"name" "a"
             "in" "query"
             "style" "form"
             "allowEmptyValue" false
             "schema" {"type" "null"}}])]
      (is
       (= "Empty value not allowed for parameter: allowEmptyValue is false"
          (get-in result ["a" :apex/error :apex.error/message])))))

  (testing "default value of allowEmptyValue"
    (let [result
          (process-query-string
           "a"
           [{"name" "a"
             "in" "query"
             "style" "form"
             "schema" {"type" "null"}}])]
      (is
       (= "Empty value not allowed for parameter: allowEmptyValue is false"
          (get-in result ["a" :apex/error :apex.error/message])))))

  (testing "Coerce nil value to empty string"
    (let [result (process-query-string
                  "a"
                  [{"name" "a"
                    "in" "query"
                    "style" "form"
                    "allowEmptyValue" true
                    "schema" {"type" "string"}}])]
      (is
       (= "" (get-in result ["a" :value])))))

  (testing "Error when empty value for string parameter"
    (let [result (process-query-string
                  "a"
                  [{"name" "a"
                    "in" "query"
                    "style" "form"
                    "allowEmptyValue" false
                    "schema" {"type" "string"}}])]
      (is
       (=
        "Empty value not allowed for parameter: allowEmptyValue is false"
        (get-in result ["a" :apex/error :apex.error/message])))))

  (testing "Error when empty value for array parameter"
    (let [result
          (process-query-string
           "a=&a=foo"
           [{"name" "a"
             "in" "query"
             "style" "form"
             "allowEmptyValue" false
             "schema" {"type" "array"
                       "items" {"type" "string"}}}])]
      (is
       (= "Empty values not allowed for parameter: allowEmptyValue is false"
          (get-in result ["a" :apex/error :apex.error/message])))))

  (testing "default value for allowEmptyValue"
    (let [result
          (process-query-string
           "a=&a=foo"
           [{"name" "a"
             "in" "query"
             "style" "form"
             "schema" {"type" "array"
                       "items" {"type" "string"}}}])]
      (is
       (= "Empty values not allowed for parameter: allowEmptyValue is false"
          (get-in result ["a" :apex/error :apex.error/message])))))

  (testing "No error when empty value for array parameter when value for allowEmptyValue is true"
    (let [result
          (process-query-string
           "a=&a=foo"
           [{"name" "a"
             "in" "query"
             "style" "form"
             "allowEmptyValue" true
             "schema" {"type" "array"
                       "items" {"type" "string"}}}])]
      (is (empty? (keep :apex/error (vals result))))
      (is (= ["" "foo"] (get-in result ["a" :value])))))

  (testing "treat empty param in object as boolean true"
    (let [result
          (process-query-string
           "R&B"
           [{"name" "color"
             "in" "query"
             "style" "form"
             "allowEmptyValue" true
             "schema" {"type" "object"
                       "properties" {"R" {"type" "boolean"}
                                     "G" {"type" "boolean"}
                                     "B" {"type" "boolean"}}}}])]
      (is (= {"R" true "B" true} (get-in result ["color" :value])))))


  (testing "treat empty param in object as empty string"
    (let [result
          (process-query-string
           "R&B=100"
           [{"name" "color"
             "in" "query"
             "style" "form"
             "allowEmptyValue" true
             "schema" {"type" "object"
                       "properties" {"R" {"type" "string"}
                                     "G" {"type" "boolean"}
                                     "B" {"type" "integer"}}}}])]
      (is (= {"R" "" "B" 100} (get-in result ["color" :value]))))))

;; TODO: Search for TODOs in parameters.clj

(deftest boolean-coercion-test
  (testing "boolean values are coerced from their query parameter value counterparts"
    (are [qs expected]
        (= expected
           (get-in
            (process-query-string
             qs
             [{"name" "a"
               "in" "query"
               "style" "form"
               "allowEmptyValue" true
               "schema" {"type" "boolean"}}])
            ["a" :value]))
        "a" true  ; when a parameter appears, even though empty, it is
                                        ; considered to indiciate 'true'
        "a=" true
        "a=b" true                    ; any value is considered 'true'
        "a=true" true
        "a=True" true
        "a=TRUE" true
        "a=yes" true
        "a=Yes" true

        ;; Here we specify a number of values that would intuitively
        ;; interpret the query parameter value as 'false', under the
        ;; Principle of Least Surprise. We're using English for
        ;; 'yes'/'no' - localisation is unlikely to be considered due
        ;; to the complexity it would involve.

        "a=false" false
        "a=False" false
        "a=FALSE" false
        "a=nil" false
        "a=Nil" false
        "a=NIL" false
        "a=no" false
        "a=No" false
        "a=NO" false)

    ;; This is the same test as above with a false allowEmptyValue. We
    ;; test the empty value errors below.
    (are [qs expected]
        (= expected
           (get-in
            (process-query-string
             qs [{"name" "a"
                  "in" "query"
                  "style" "form"
                  "allowEmptyValue" false
                  "schema" {"type" "boolean"}}])
            ["a" :value]))
        "a=b" true
        "a=true" true
        "a=True" true
        "a=TRUE" true
        "a=yes" true
        "a=Yes" true

        "a=false" false
        "a=False" false
        "a=FALSE" false
        "a=nil" false
        "a=Nil" false
        "a=NIL" false
        "a=no" false
        "a=No" false
        "a=NO" false))

  (testing
      "ensure expected errors on empty values if allowEmptyValue is false"
      (are [qs]
          (= "Empty value not allowed for parameter: allowEmptyValue is false"
             (get-in
              (process-query-string
               qs [{"name" "a"
                    "in" "query"
                    "style" "form"
                    "allowEmptyValue" false
                    "schema" {"type" "boolean"}}])
              ["a" :apex/error :apex.error/message]))
          "a"
          "a=")))

(deftest integer-coercion-test
  (is
   (let [result (process-query-string
                 "a=100"
                 [{"name" "a"
                   "in" "query"
                   "style" "form"
                   "schema" {"type" "integer"}}])]
     (= 100 (get-in result ["a" :value]))))

  (testing "Illegal integer format returns an error"
    (is (get-in (process-query-string
                 "a=10d0"
                 [{"name" "a"
                   "in" "query"
                   "style" "form"
                   "schema" {"type" "integer"}}])
                ["a" :apex/error]))))

;; TODO: Add specs which will help to drive thinking about consistent data


;; TODO: Error handling for query parameters (something similar to
;; cognitect anomolies) - extend out to replace the records used in
;; path-parameters

;; TODO: Formatting for query parameters

;; TODO; Support content

;; TODO: Support header parameters

(deftest required-parameter-test
  (are [type explode]
      (let [result
            (process-query-string
             "a=10"
             [{"name" "b"
               "in" "query"
               "schema" {"type" type}
               "explode" explode
               "required" true}])]
        (= {:apex.error/message "Required parameter missing"}
           (get-in result ["b" :apex/error])))
      "string" true
      "string" false
      "array" true
      "array" false
      "object" false
      ;; This test is n/a for exploded objects since the object may be
      ;; empty (it has no required properties). The schema validation
      ;; which ensure that we get an error if any required properties
      ;; are not contained in the query-string.
      ))

(deftest pipe-delimited-test
  (testing "when explode is false, only first query parameter is considered"
    (testing "arrays"
      (let [result
            (process-query-string
             "color=R|100|G|200&color=B|150|Z|100"
             [{"name" "color"
               "in" "query"
               "style" "pipeDelimited"
               "schema" {"type" "array"
                         "items" {"type" "string"}}
               "explode" false}])]
        (is (= ["R" "100" "G" "200"] (get-in result ["color" :value])))))

    (testing "objects"
      (let [result
            (process-query-string
             "color=R|100|G|200&color=B|150|Z|100"
             [{"name" "color"
               "in" "query"
               "style" "pipeDelimited"
               "schema" {"type" "object"
                         "properties" {"R" {"type" "string"}
                                       "G" {"type" "integer"}
                                       }}
               "explode" false}])]
        (is (= {"R" "100", "G" 200} (get-in result ["color" :value]))))))

  (testing "when explode is true, all values are separate parameters"
    (testing "arrays"
      (let [result
            (process-query-string
             "color=R|100|G|200&color=B|150|Z|100"
             [{"name" "color"
               "in" "query"
               "style" "pipeDelimited"
               "schema" {"type" "array"
                         "items" {"type" "string"}}
               "explode" true}])]
        (is (= ["R|100|G|200" "B|150|Z|100"] (get-in result ["color" :value])))))

    (testing "objects"
      (let [result
            (process-query-string
             ;; Here the key-value pair is split according to the style
             "color=R|100&color=G|200"
             [{"name" "color"
               "in" "query"
               "style" "pipeDelimited"
               "schema" {"type" "object"
                         "properties" {"R" {"type" "string"}
                                       "G" {"type" "integer"}
                                       }}
               "explode" true}])]
        (is (= {"R" "100", "G" 200} (get-in result ["color" :value])))))))

(deftest space-delimited-test
  (testing "when explode is false, only first query parameter is considered"
    (testing "arrays"
      (let [result
            (process-query-string
             "color=R 100 G 200&color=B 150 Z 100"
             [{"name" "color"
               "in" "query"
               "style" "spaceDelimited"
               "schema" {"type" "array"
                         "items" {"type" "string"}}
               "explode" false}])]
        (is (= ["R" "100" "G" "200"] (get-in result ["color" :value])))))

    (testing "objects"
      (let [result
            (process-query-string
             "color=R 100 G 200&color=B 150 Z 100"
             [{"name" "color"
               "in" "query"
               "style" "spaceDelimited"
               "schema" {"type" "object"
                         "properties" {"R" {"type" "string"}
                                       "G" {"type" "integer"}}}
               "explode" false}])]
        (is (= {"R" "100", "G" 200} (get-in result ["color" :value]))))))

  (testing "when explode is true, all values are separate parameters"
    (testing "arrays"
      (let [result
            (process-query-string
             "color=R 100 G 200&color=B 150 Z 100"
             [{"name" "color"
               "in" "query"
               "style" "spaceDelimited"
               "schema" {"type" "array"
                         "items" {"type" "string"}}
               "explode" true}])]
        (is (= ["R 100 G 200" "B 150 Z 100"] (get-in result ["color" :value])))))

    (testing "objects"
      (let [result
            (process-query-string
             ;; Here the key-value pair is split according to the style
             "color=R 100&color=G 200"
             [{"name" "color"
               "in" "query"
               "style" "spaceDelimited"
               "schema" {"type" "object"
                         "properties" {"R" {"type" "string"}
                                       "G" {"type" "integer"}
                                       }}
               "explode" true}])]
        (is (= {"R" "100", "G" 200} (get-in result ["color" :value])))))))

(deftest deep-object-test
  (let [result
        (process-query-string
         "color[R]=100&color[G]=200&color[B]=150"
         [{"name" "color"
           "in" "query"
           "style" "deepObject"
           "schema" {"type" "object"
                     "properties" {"R" {"type" "integer"}
                                   "G" {"type" "integer"}
                                   "B" {"type" "integer"}}}
           "explode" true}])]
    (is
     (= {"R" 100, "G" 200, "B" 150} (get-in result ["color" :value] )))))

;; Complex Scenarios

;; "For more complex scenarios, the content property can define the
;; media type and schema of the parameter. A parameter MUST contain
;; either a schema property, or a content property, but not both.

(deftest complex-situation-test
  (let [result
        (process-query-string
         (str "coordinates=" (codec/url-encode "{\"lat\": 52.1, \"long\": 0.45}"))
         [{"name" "coordinates"
           "in" "query"
           "content"
           {"application/json"
            {"schema"
             {"type" "object"
              "required" ["lat" "long"]
              "properties" {"lat" {"type" "number"}
                            "long" {"type" "number"}}}}}
           "explode" false}])]
    (is (=
         {"long" 0.45, "lat" 52.1}
         (get-in result ["coordinates" :value])))))

;; TODO: Test form encoding

;; Path parameters

(deftest path-params-test
  (testing "string schema"
    (let [params
          (process-path-parameters
           {:petId "3"}
           [{"name" "petId" "required" true "schema" {"type" "string"}}])
          result (get params "petId")]
      (is result)
      (is (= "3" (:value result)))
      (is (:validation result))
      (is (get-in result [:validation :valid?])))))

(deftest path-param-coercion-test
  (testing "integer schema with coercion"
    (let [params
          (process-path-parameters
           {:petId "3"}
           [{"name" "petId" "required" true "schema" {"type" "integer"}}])
          result (get params "petId")]
      (is result)
      (is (= 3 (:value result)))
      (is (:validation result))
      (is (get-in result [:validation :valid?])))))

(deftest error-param-test
  (testing "integer schema without coercion"
    (let [params
          (process-path-parameters
           {:petId "3"}
           [{"name" "petId" "required" true "schema" {"type" "integer"}}]
           {:coercions {}})
          result (get params "petId")]

      (is (= #{"petId"} (into #{} (keys params))))
      (is (:apex/error result))
      (is (= "Path parameter not valid according to schema"
             (get-in result [:apex/error :apex.error/message]))))))

;; TODO:
;; https://swagger.io/specification/#parameterStyle:
;;  "If the parameter location is "path", this property is REQUIRED
;;  and its value MUST be true. Otherwise, the property MAY be
;;  included and its default value is false."
(deftest missing-required-parameter-test
  (testing "Error is required parameter is missing"
    (let [params
          (process-path-parameters
           {}
           [{"name" "petId" "required" true "schema" {"type" "string"}}])
          result (get params "petId")]
      (is result)
      (is (:apex/error result))
      (is (= "Required parameter not found" (get-in result [:apex/error :apex.error/message])))
      ;; We shhould want to get the parameter details, ensure they're in the result
      (is (:param result))
      ))
  (testing "OK if missing parameter is not required"
    (let [params
          (process-path-parameters
           {}
           [{"name" "petId" "required" false "schema" {"type" "string"}}])
          result (get params "petId")]
      (is result)
      (is (nil? (find result :apex/error)))

      )))

;; TODO: parameter errors (plus dev middleware that produces nice 400 page with actual explain breakdown of each error)

;; TODO: Resolve 'Reference Objects' in OpenAPI docs

;; TODO: Figure out differences between OpenAPI and json-schema
;; required for passing to jinx. Possibly run jinx in a 'mode' where
;; it understands the OpenAPI dialect.

;; TODO: header params



;; TODO: cookie params

;; TODO: Write implementation nodes for module (parameters.adoc) and
;; chapter documentation before moving on - there may be further errors that
;; can be sniffed out and better design (documentation driven design).

;; TODO: Restore working website with parameter examples
