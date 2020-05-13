(ns crux.cms.db
  (:require
   [crux.api :as crux]
   [integrant.core :as ig]
   [clojure.java.io :as io]))

#_(new String (.decode (java.util.Base64/getDecoder) (new String (.encode (java.util.Base64/getEncoder) (.getBytes "foobar")))))

#_(defn b64-encode [])
#_(new String (.encode (java.util.Base64/getEncoder) (.getBytes "foobar")))

(def WEBSITE_REPO_DIR (io/file (System/getProperty "user.home") "src/github.com/juxt/website"))

(def DB_DIR (io/file "db"))

(io/make-parents DB_DIR)

(defmethod ig/init-key ::node [_ _]
  (let [node
        (crux/start-node
         {:crux.node/topology '[crux.standalone/topology]
          :crux.kv/db-dir (str DB_DIR)
          :crux.standalone/event-log-sync? false})]

    node))

(defmethod ig/halt-key! ::node [_ node]
  (.close node))


#_(new String (.decode (java.util.Base64/getDecoder) (new String (.encode (java.util.Base64/getEncoder) (.getBytes "foobar")))))
