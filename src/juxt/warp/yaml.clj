(ns juxt.warp.yaml
  (:require
   [clj-yaml.core :as yaml]))

(alter-var-root #'clj-yaml.core/*keywordize* (constantly false))

(defn parse-string [s]
  (yaml/parse-string s nil))
