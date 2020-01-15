;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.alpha.openapi.yaml
  (:require
   [clj-yaml.core :as yaml]))

(alter-var-root #'clj-yaml.core/*keywordize* (constantly false))

(defn parse-string [s]
  (yaml/parse-string s nil))
