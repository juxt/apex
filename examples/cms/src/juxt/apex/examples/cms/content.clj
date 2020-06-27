(ns juxt.apex.examples.cms.content
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]))

(def WEBSITE_REPO_DIR (io/file (System/getProperty "user.home") "src/github.com/juxt/website"))

(defn slurp-file-as-b64encoded-string [f]
  (try
    (let [bytes (.readAllBytes (new java.io.FileInputStream f))]
      {:apex.http/content (.encodeToString (java.util.Base64/getEncoder) bytes)
       :apex.http/content-length (count bytes)
       :apex.http/content-coding :base64
       :apex.http/last-modified (java.util.Date. (.lastModified f))
       :apex.http/content-source (.toURI f)})
    (catch Throwable t
      (throw (ex-info "Failed to load file" {:file f} t)))))

(defn slurp-file-as-string [f]
  (try
    {:apex.http/content (slurp f)
     :apex.http/content-length (.length f)
     :apex.http/last-modified (java.util.Date. (.lastModified f))
     :apex.http/content-source (.toURI f)}
    (catch Throwable t
      (throw (ex-info "Failed to load file" {:file f} t)))))

(defn ingest-content [tx]
  (cond-> tx
    (and
     (not (:apex.http/content tx))
     (:apex.http/content-source tx))
    (merge
     (let [f (io/file (:apex.http/content-source tx))]
       (case (:apex.http/content-coding tx)
         :base64
         (slurp-file-as-b64encoded-string f)
         (slurp-file-as-string f))))))

(defn compute-content-length
  "Where no content-length already exists, add it."
  [tx]
  (cond-> tx
    (and
     (not (:apex.http/content-length tx))
     (:apex.http/content tx)
     (not (:apex.http/content-coding tx)))
    (assoc :apex.http/content-length (.length (:apex.http/content tx)))))

(defn compute-etag [tx]
  ;; If there _is_ content,
  (cond-> tx
    (and
     (not (:apex.http/entity-tag tx))         ; no pre-existing entity-tag
     (:apex.http/content tx)             ; but some content
     )
    (assoc
     :apex.http/entity-tag
     (hash
      (select-keys
       tx
       [:apex.http/content ; if the content changed, the etag would too
        :apex.http/content-encoding
        :apex.http/content-language
        :apex.http/content-type])))))

(defn content-txes []
  (map compute-etag
       (remove nil?
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
                      :apex.http/content-type "text/plain;charset=utf-8"
                      :apex.http/content-language "en"}
                     (slurp-file-as-string f))))))))
