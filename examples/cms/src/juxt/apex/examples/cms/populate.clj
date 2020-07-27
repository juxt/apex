;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.examples.cms.populate
  (:require
   [crux.api :as crux]
   [jsonista.core :as json]
   [integrant.core :as ig]
   [juxt.apex.examples.cms.content :as content]
   [clojure.set :as set]))

(defmethod ig/init-key ::seeder [_ {:keys [node]}]
  (println "Seeding database")
  (crux/submit-tx
   node
   (for [tx (content/content-txes)]
     [:crux.tx/put tx])))

(comment
  (take 20
        (for [record (json/read-value (slurp "data/people.json"))
              :let [ent (set/rename-keys record {"first_name" :person/name
                                                 "email" :person/email})]]
          [:crux.tx/put (into
                         {:crux.db/id (keyword (str "person-" (get record "id")))}
                         (select-keys ent [:person/name :person/email]))])))
