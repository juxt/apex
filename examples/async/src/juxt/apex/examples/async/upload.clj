;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.examples.async.upload
  (:require
   [juxt.apex.examples.async.async-helpers :refer [h pipe-to-file]]))

(defn upload-file-example [req respond raise]
  (let [vertx (:apex.vertx/vertx req)
        vertx-request (:apex.vertx/request req)]

    (.setExpectMultipart vertx-request true)

    ;; NOTE: HTTP/2 supports stream reset at any time during the
    ;; request/response -- https://vertx.io/docs/vertx-core/java/ --
    ;; which is great for telling our CMS clients that we already have
    ;; a given file. Test with --http2

    (. vertx-request
       uploadHandler
       (h (fn [upload]
            (pipe-to-file
             vertx
             upload
             (str "COPY3-" (.filename upload))
             {:on-success (fn [file]
                            (respond
                             {:status 200
                              :body (format "Thanks! Bytes received: %s\n" (.getWritePos file))}))
              :on-failure raise}))))))
