(ns crux.cms.db.populate)

#_(crux/submit-tx
 node

 ;; Selmer templates
 (concat
  (let [dir (io/file WEBSITE_REPO_DIR "juxt.website/resources/templates")]
    (for [f (file-seq dir)
          :when (.isFile f)
          :let [p (str (.relativize (.toPath dir) (.toPath f)))]]
      [:crux.tx/put
       {:crux.db/id (java.net.URI. (str "http://localhost:8000/templates/" p))
        :content (slurp f)
        }]))

  [
   ;; This is the document
   [:crux.tx/put
    {:crux.db/id (java.net.URI. "http://localhost:8000/index.html")
     :template (java.net.URI. "http://localhost:8000/templates/index2.html")
     }]

   ;; A blob (like an image)
   #_[:crux.tx/put
      {:crux.db/id (java.net.URI. "http://localhost:8000/foo.jpg")
       :content (.encode (java.util.Base64/getEncoder) (.getBytes "foobar"))
       }]]))
