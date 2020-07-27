;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.examples.cms.graphql.seeder
  (:require
   [crux.api :as crux]
   [jsonista.core :as json]
   [integrant.core :as ig]
   [clojure.set :as set]))

(defn data []
  (let [records (json/read-value (slurp "data/people.json"))]
    (for [record records
          :let [ent (set/rename-keys record {"first_name" :person/name
                                             "email" :person/email})]]
      (into
       {:crux.db/id (keyword (str "person-" (get record "id")))}
       (select-keys ent [:person/name :person/email])))))

(defmethod ig/init-key ::data [_ {:keys [node]}]
  (let [data (data)]
    (printf "Seeding database with %d person records\n" (count data))
    (crux/submit-tx
     node
     (for [ent data]
       [:crux.tx/put ent]))))
