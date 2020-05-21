;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.alpha.async.flowable
  (:refer-clojure :exclude [map range])
  (:import
   (io.vertx.reactivex.core.buffer Buffer)
   (io.reactivex Flowable)
   (io.reactivex.functions Function)
   (java.util.concurrent TimeUnit)))

(defn range [^long start ^long count]
  (Flowable/range start count))

(defn map [f flowable]
  (.map flowable (reify Function
                   (apply [_ t]
                     (f t)))))

(defn server-sent-event [s]
  (Buffer/buffer (format "data: %s\r\n\r\n" s)))

(defn throttle-first [^long window-duration ^TimeUnit unit flowable]
  (.throttleFirst flowable window-duration unit))
