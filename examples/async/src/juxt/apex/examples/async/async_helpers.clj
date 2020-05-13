;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.examples.async.async-helpers
  (:import
   (io.vertx.core Handler)))

(defn h [cb]
  (reify Handler
    (handle [_ t]
      (cb t))))

(defn har [{:keys [on-success on-failure]}]
  (h (fn [ar]
       (if (. ar succeeded)
         (on-success (. ar result))
         (on-failure (. ar cause))))))

(defn wrap-failure
  "Wrap failure to add additional context. The wrapper parameter
  typically returns a clojure.lang.ExceptionInfo."
  [wrapper on-failure]
  (fn [cause]
    (on-failure (wrapper cause))))

(defn pipe-to-file
  "Pipe the source to the filename"
  [vertx source filename
   {:keys [on-success                ; receives AsyncFile as parameter
           on-failure]}]
  ;; Create the pipe now, don't start streaming from an
  ;; on-success callback, otherwise the initial bytes will
  ;; get dropped.
  (let [pipe (. source pipe)]
    (.
     (. vertx fileSystem)
     open
     filename
     (new io.vertx.core.file.OpenOptions)
     (har
      {:on-success                      ; of open
       (fn [file]
         (. pipe to file
            (har
             {:on-success               ; of write
              (fn [_]
                (on-success file))
              :on-failure               ; to write
              (wrap-failure
               (fn [cause]
                 (ex-info
                  (format "Failed writing to %s after %s bytes"
                          filename (.getWritePos file))
                  {:filename filename
                   :bytes-written (.getWritePos file)}
                  cause))
               on-failure)})))
       :on-failure                      ; to open
       (wrap-failure
        (fn [cause]
          (ex-info
           (str "Failed to open file: " filename)
           {:filename filename}
           cause))
        on-failure)}))))
