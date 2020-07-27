;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.examples.cms.graphql.eql
  (:refer-clojure :exclude [ident?])
  (:require
   [crux.api :as crux]
   [edn-query-language.core :as eql]
   [juxt.apex.examples.cms.graphql.seeder :as seeder]
   )
  )

(def ident? vector?)

(defn lookup [db k]
  (if (ident? k)
    (let [[attr val] k]
      (let [candidates
            (map first
                 (crux/q db {:find ['?e]
                             :args [{'val val}]
                             :where [['?e attr 'val]]}))]
        (when (not= (count candidates) 1)
          (throw
           (ex-info
            "Multiple entries found for ident"
            {:ident k})))

        (crux/entity db (first candidates))))
    (crux/entity db k)))

(defmulti query (fn [db e ast opts] (:type ast)))

(defmethod query nil [db e ast opts]
  {:error "no type"
   :ast ast})

(defmethod query :root [db e ast opts]
  (mapv #(query db nil % opts) (:children ast)))

(defmethod query :prop [db e ast opts]
  {(:key ast)
   (or (get e (:key ast))
       (get-in ast [:params :default])
       {:error "prop not found"
        :k (:key ast)
        :ast ast
        :e e})})

(defmethod query :join [db e ast opts]
  (if-let [resolver (get-in ast [:params :resolver])]
    (if-let [q (:crux/query resolver)]
      (let [q (cond-> q
                true (update
                      :args (fn [args]
                              (mapv (fn [arg]
                                      (merge
                                       ;; Default args
                                       {;;'% (get e (:dispatch-key ast))
                                        '?subject (:subject opts)
                                        '?resource (:crux.db/id e)
                                        '?action (:action opts)}
                                       ;; Process args against opts
                                       ;; e.g. ?subject gets the value of (:subject opts)
                                       (into {}
                                             (map (fn [[k v]]
                                                    [k (get opts (keyword (subs (str v) 1)) v)])
                                                  arg))))
                                    (if (empty? args) [{}] args))))
                ;;true (update :where conj '(authorized? ?subject ?resource ?action))
                (:rules opts) (update :rules (comp vec concat) (:rules opts)))]
        {(:dispatch-key ast)
         (case (:debug resolver)
           :dry-run {:crux/query q}
           :eids {:eids (mapv first (crux/q db q))}
           :embed-query
           (let [result (vec (for [eid (map first (crux/q db q))
                                   :let [e (lookup db eid)]]
                               (apply merge (map #(query db e % opts) (:children ast)))))]
             (if (:debug resolver)
               {:crux/query q
                :e e
                :result result}))

           ;; default case
           (vec (for [eid (map first (crux/q db q))
                      :let [e (lookup db eid)]]
                  (apply merge (map #(query db e % opts) (:children ast))))))})

      (throw (ex-info "No query specified in resolver!" {})))

    ;; TODO: Wow, this is dangerous because it bypasses authz!
    (if-let [e (if e
                 (if-let [eid-maybe (get e (:key ast))]
                   (or (lookup db eid-maybe) eid-maybe)
                   {:error "prop doesn't exist"
                    :key (:key ast)})
                 (lookup db (:key ast)))]
      {(:dispatch-key ast)
       (apply merge (map #(query db e % opts) (:children ast)))}
      {:error "Not found"
       :key (:key ast)
       :e e})))

(defn load-data [crux ents]
  (crux/await-tx
   crux
   (crux/submit-tx
    crux
    (vec
     (for [ent ents]
       [:crux.tx/put ent])))))

(def eql-query
  '[{(:drivers
      {:resolver
       {:crux/query
        {:find [?p]
         :where [[?p :person/name ?name]]}
        :debug false}})
     [:person/name :person/email]}])

(comment
  (with-open
    [node (crux/start-node {:crux.node/topology '[crux.standalone/topology]})]
    (load-data
     node
     (seeder/data))
    (let [db (crux/db node)
          ast (eql/query->ast eql-query)]
      (query db nil ast {}))))
