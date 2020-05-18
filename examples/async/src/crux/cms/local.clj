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
      {:readers {'crux/uri (fn [x] (java.net.URI. x))
                 'crux.cms/include (fn [path] (slurp (io/file WEBSITE_REPO_DIR path)))
                 'crux.cms/file (fn [path]
                                  (.getAbsolutePath (io/file WEBSITE_REPO_DIR path)))}}
      (slurp "src/crux/cms/local/content.edn")))

    ;; Selmer template sources
    (let [dir (io/file WEBSITE_REPO_DIR "juxt.website/resources/templates")]
      (for [f (file-seq dir)
            :when (.isFile f)
            :let [p (str (.relativize (.toPath dir) (.toPath f)))]]
        {:crux.db/id (java.net.URI. (str "https://juxt.pro/_sources/templates/" p))
         :crux.web/content-type "text/plain;charset=utf-8"
         :crux.web/content-language "en"
         :crux.cms/content (slurp f)}))

    ;; TODO: This should be the place where dependencies are detected
    ;; and placed into the graph along with the document as
    ;; references.

    ;; Sass
    (let [dir (io/file WEBSITE_REPO_DIR "juxt.website")]
      (for [[dir path]
            [[(io/file dir "src") "frontpage.scss"]
             [dir "assets/css/variables.scss"]
             ]]

        {:crux.db/id (java.net.URI. (str "https://juxt.pro/_sources/sass/" path))
         :crux.web/content-type "text/plain;charset=utf-8"
         :crux.web/content-language "en"
         :crux.cms/content (slurp (io/file dir path))})))

   (map (juxt :crux.db/id identity))
   (into {})
   (new LocalContentStore)))
