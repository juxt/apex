;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.doc-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [juxt.apex.yaml :as yaml]
   [clojure.java.io :as io]
   [juxt.apex.doc :as doc]))


(deftest compile-path-template-test
  (is
   (= {"petId" "123/3423", "page" "1"}
      ((doc/compile-path-template "/pets/{petId}/pages/{page}") "/pets/123/3423/pages/1"))))

(deftest matcher-test
  (let [doc (yaml/parse-string
             (slurp (io/resource "juxt/apex/openapi-examples/petstore.yaml")))
        doc (doc/process-document doc)
        matcher (-> doc (get-in ["paths" "/pets/{petId}"]) meta (get :apex/matcher))]
    (is (= (matcher "/pets/123") {"petId" "123"}))))

(deftest path-for-test
  (let [doc (yaml/parse-string
             (slurp (io/resource "juxt/apex/openapi-examples/petstore.yaml")))
        params {"petId" 123}]
    (is (= "/pets/123" (doc/path-for doc "showPetById" params)))
    (is (= "/pets" (doc/path-for doc "listPets" params)))))
