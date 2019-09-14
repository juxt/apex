;; Copyright © 2019, JUXT LTD.

(ns juxt.apex.alpha2.parameters-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [juxt.apex.alpha2.parameters :refer [process-path-parameters]])
  (:import
   (juxt.apex.alpha2.parameters
    RequiredParameterMissingError
    ParameterSchemaValidationError)))

(deftest path-params-test
  (testing "string schema"
    (is
     (=
      {"petId" "3"}
      (process-path-parameters
       {:petId "3"}
       {"petId" {:required? true :schema {"type" "string"}}}
       )))))

(deftest path-param-coercion-test
  (testing "integer schema with coercion"
    (is
     (=
      {"petId" 3}
      (process-path-parameters
       {:petId "3"}
       {"petId" {:required? true :schema {"type" "integer"}}}
       )))))

(deftest error-param-test
  (testing "integer schema without coercion"
    (let [result
          (process-path-parameters
           {:petId "3"}
           {"petId" {:required? true :schema {"type" "integer"}}}
           {:coercions {}})
          error (get result "petId")]

      (is (= #{"petId"} (into #{} (keys result))))
      (is (instance? ParameterSchemaValidationError error))
      (is (not (:valid? error)))
      (is (= "Instance of \"3\" is not of type \"integer\""
             (get-in error [:errors 0 :message]))))))

(deftest missing-required-parameter-test
  (testing "Error is required parameter is missing"
    (let [result
          (process-path-parameters
           {}
           {"petId" {:required? true :schema {"type" "string"}}}
           )
          error (get result "petId")]
      (is (instance? RequiredParameterMissingError error))))
  (testing "OK if missing parameter is not required"
    (let [result
          (process-path-parameters
           {}
           {"petId" {:required? false :schema {"type" "string"}}}
           )]
      (is (nil? (get result "petId"))))))
