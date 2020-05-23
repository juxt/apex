(ns juxt.apex.examples.cms.content
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]))

(def WEBSITE_REPO_DIR (io/file (System/getProperty "user.home") "src/github.com/juxt/website"))
(def PLAN_REPO_DIR (io/file (System/getProperty "user.home") "src/github.com/juxt/plan"))

(defn content-txes []
  (->>
   (concat

    ;; Content
    (->>
     (edn/read-string
      {:readers {'crux/uri (fn [x] (java.net.URI. x))
                 'crux.cms/include (fn [path] (slurp (io/file WEBSITE_REPO_DIR path)))
                 'crux.cms/file (fn [path]
                                  (.getAbsolutePath (io/file WEBSITE_REPO_DIR path)))}}
      (slurp "src/juxt/apex/examples/cms/content.edn")))

    ;; Selmer template sources
    (let [dir (io/file WEBSITE_REPO_DIR "juxt.website/resources/templates")]
      (for [f (file-seq dir)
            :when (.isFile f)
            :let [p (str (.relativize (.toPath dir) (.toPath f)))]]
        {:crux.db/id (java.net.URI. (str "https://juxt.pro/_sources/templates/" p))
         :crux.web/content-type "text/plain;charset=utf-8"
         :crux.web/content-language "en"
         :crux.web/last-modified (java.util.Date. (.lastModified f))
         :crux.cms/content (slurp f)}))

    ;; Plan repo site sources
    (let [dir (io/file PLAN_REPO_DIR "site")]
      (for [f (file-seq dir)
            :when (.isFile f)
            :let [p (str (.relativize (.toPath dir) (.toPath f)))]
            :when (not (.startsWith p "."))]
        (merge
         {:crux.cms/content-source (.toURI f)
          :crux.web/last-modified (java.util.Date. (.lastModified f))}
         (condp re-matches p
           #".*\.adoc"
           {:crux.db/id (java.net.URI. (str "https://juxt.pro/_sources/plan/site/" p))
            :crux.web/content-type "text/plain;charset=utf-8"
            :crux.web/content-language "en"
            :crux.cms/content (slurp f)}
           #".*\.svg"
           {:crux.db/id (java.net.URI. (str "https://juxt.pro/" p))
            :crux.web/content-type "image/svg+xml"
            :crux.web/content-language "en"
            :crux.cms/content (slurp f)
            :crux.ac/classification :public}
           #".*\.png"
           {:crux.db/id (java.net.URI. (str "https://juxt.pro/" p))
            :crux.web/content-type "image/png"
            :crux.web/content-coding :base64
            :crux.cms/content (.encodeToString (java.util.Base64/getEncoder) (.readAllBytes (new java.io.FileInputStream f)))
            :crux.ac/classification :public
            }))))

    ;; TODO: This should be the place where dependencies are detected
    ;; and placed into the graph along with the document as
    ;; references.

    ;; Sass
    (let [dir (io/file WEBSITE_REPO_DIR "juxt.website")]
      (for [[dir path]
            [[(io/file dir "src") "frontpage.scss"]
             [dir "assets/css/variables.scss"]
             ]]
        (let [f (io/file dir path)]

          {:crux.db/id (java.net.URI. (str "https://juxt.pro/_sources/sass/" path))
           :crux.web/content-type "text/plain;charset=utf-8"
           :crux.web/content-language "en"
           :crux.web/last-modified (java.util.Date. (.lastModified f))
           :crux.cms/content (slurp f)
           :crux.ac/classification :public}))))))
