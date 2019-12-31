;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.resolve-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [jsonista.core :as j]
   [juxt.apex.request :refer [handler]]
   [juxt.apex.yaml :as yaml]
   [ring.mock.request :as mock]
   [juxt.jinx-alpha.validate :as v]))




(let [document (yaml/parse-string (slurp (io/resource "juxt/apex/openapi-examples/petstore.yaml")))
      schema (get-in document ["paths" "/pets" "get" "responses" "200" "content" "application/json" "schema"])]
  (v/validate
   [{"id" 2 "name" "Rover"}
    {"id" 1 "name" "Rover"}]
   schema
   {:base-document document
    })
  )

;; Get some schema out of document
;; Resolve references to schema
