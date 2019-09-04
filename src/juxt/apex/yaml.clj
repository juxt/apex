;; Copyright © 2019, JUXT LTD.

(ns juxt.apex.yaml
  (:require
   [clj-yaml.core :as yaml]))

(alter-var-root #'clj-yaml.core/*keywordize* (constantly false))

(defn parse-string [s]
  (yaml/parse-string s nil))
