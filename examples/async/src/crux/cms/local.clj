(ns crux.cms.local
  (:require
   [crux.cms.cms :as cms]
   [integrant.core :as ig]
   [clojure.edn :as edn]
   [clojure.java.io :as io]))

;; This is for development, but will be quite quickly be replaced by a
;; local Crux database for development once things are stable.

(defrecord LocalContentStore [entities]
  cms/ContentStore
  (find-entity [_ id]
    (println "LocalContentStore: find-entity" id)
    (get entities id)))

(def WEBSITE_REPO_DIR
  (io/file
   (System/getProperty "user.home")
   "src/github.com/juxt/website"))

(defmethod ig/init-key ::content-store [_ _]
  (println "Creating local content store")
  (->>
   (concat

    ;; Content
    (->>
     (edn/read-string
      {:readers {'crux/uri (fn [x] (java.net.URI. x))}} (slurp "src/crux/cms/local/content.edn")))

    (let [dir (io/file WEBSITE_REPO_DIR "juxt.website/resources/templates")]
      (for [f (file-seq dir)
            :when (.isFile f)
            :let [p (str (.relativize (.toPath dir) (.toPath f)))]]
        {:crux.db/id (java.net.URI. (str "https://juxt.pro/_sources/templates/" p))
         :crux.web/content-type "text/plain;charset=utf-8"
         :crux.web/content-language "en"
         :crux.cms/content (slurp f)})))
   (map (juxt :crux.db/id identity))
   (into {})
   (new LocalContentStore)))
