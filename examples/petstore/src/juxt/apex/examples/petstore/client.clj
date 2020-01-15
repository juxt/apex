;; Copyright © 2020, JUXT LTD.
;;

(ns juxt.apex.examples.petstore.client
  (:require [integrant.core :as ig]))

(defmethod ig/init-key :juxt.apex.examples.petstore.client/seed [_ _]
  (println "seeding!")
  )
