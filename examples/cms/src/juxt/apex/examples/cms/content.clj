(ns juxt.apex.examples.cms.content
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]))

(def WEBSITE_REPO_DIR (io/file (System/getProperty "user.home") "src/github.com/juxt/website"))
(def PLAN_REPO_DIR (io/file (System/getProperty "user.home") "src/github.com/juxt/plan"))

(defn slurp-file-as-b64encoded-string [f]
  (let [bytes (.readAllBytes (new java.io.FileInputStream f))]
    {:crux.web/content (.encodeToString (java.util.Base64/getEncoder) bytes)
     :crux.web/content-length (count bytes)
     :crux.web/content-coding :base64
     :crux.web/last-modified (java.util.Date. (.lastModified f))
     :crux.cms/content-source (.toURI f)}))

(defn slurp-file-as-string [f]
  {:crux.web/content (slurp f)
   :crux.web/content-length (.length f)
   :crux.web/last-modified (java.util.Date. (.lastModified f))
   :crux.cms/content-source (.toURI f)})

(defn ingest-content [tx]
  (cond-> tx
    (and
     (not (:crux.web/content tx))
     (:crux.cms/content-source tx))
    (merge
     (let [f (io/file (:crux.cms/content-source tx))]
       (case (:crux.web/content-coding tx)
         :base64
         (slurp-file-as-b64encoded-string f)
         {:crux.web/content (slurp f)
          :crux.web/content-length (.length f)})))))

(defn compute-content-length
  "Where no content-length already exists, add it."
  [tx]
  (cond-> tx
    (and
     (not (:crux.web/content-length tx))
     (:crux.web/content tx)
     (not (:crux.web/content-coding tx)))
    (assoc :crux.web/content-length (.length (:crux.web/content tx)))))

#_(defn compute-etag [tx]
  (cond-> tx
    (:crux.web/content tx)
    (assoc :crux.web/entity-tag (hash (:crux.web/content tx)))))

(defn content-txes []
  (concat

   ;; Content
   (->>
    (map
     (comp compute-content-length ingest-content)
     (edn/read-string
      {:readers {'crux/uri (fn [x] (java.net.URI. x))
                 'crux.cms/file (fn [path]
                                  (.getAbsolutePath (io/file WEBSITE_REPO_DIR path)))}}
      (slurp "src/juxt/apex/examples/cms/content.edn"))))

   ;; Selmer template sources
   (let [dir (io/file WEBSITE_REPO_DIR "juxt.website/resources/templates")]
     (for [f (file-seq dir)
           :when (.isFile f)
           :let [p (str (.relativize (.toPath dir) (.toPath f)))]]
       (merge
        {:crux.db/id (java.net.URI. (str "https://juxt.pro/_sources/templates/" p))
         :crux.web/content-type "text/plain;charset=utf-8"
         :crux.web/content-language "en"}
        (slurp-file-as-string f))))

   ;; Plan repo site sources
   (let [dir (io/file PLAN_REPO_DIR "site")]
     (for [f (file-seq dir)
           :when (.isFile f)
           :let [p (str (.relativize (.toPath dir) (.toPath f)))]
           :when (not (.startsWith p "."))]
       (condp re-matches p
         #".*\.adoc"
         (merge
          {:crux.db/id (java.net.URI. (str "https://juxt.pro/_sources/plan/site/" p))
           :crux.web/content-type "text/plain;charset=utf-8"
           :crux.web/content-language "en"}
          (slurp-file-as-string f))

         #".*\.svg"
         (merge
          {:crux.db/id (java.net.URI. (str "https://juxt.pro/" p))
           :crux.web/content-type "image/svg+xml"
           :crux.web/content-language "en"
           :crux.ac/classification :public}
          (slurp-file-as-string f))

         #".*\.png"
         (merge
          {:crux.db/id (java.net.URI. (str "https://juxt.pro/" p))
           :crux.web/content-type "image/png"
           :crux.ac/classification :public}
          (slurp-file-as-b64encoded-string f)))))

   ;; TODO: This should be the place where dependencies are detected
   ;; and placed into the graph along with the document as
   ;; references.

   ;; Sass
   #_(let [dir (io/file WEBSITE_REPO_DIR "juxt.website")]
       (for [[dir path]
             [[(io/file dir "src") "frontpage.scss"]
              [dir "assets/css/variables.scss"]
              ]]
         (let [f (io/file dir path)]

           {:crux.db/id (java.net.URI. (str "https://juxt.pro/_sources/sass/" path))
            :crux.web/content-type "text/plain;charset=utf-8"
            :crux.web/content-language "en"
            :crux.web/last-modified (java.util.Date. (.lastModified f))
            :crux.web/content (slurp f)
            :crux.ac/classification :public})))))
