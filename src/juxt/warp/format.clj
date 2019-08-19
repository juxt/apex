(ns juxt.warp.format
  (:refer-clojure :exclude [format])
  (:require
   [muuntaja.format.core :as m])
  (:import (java.io InputStreamReader PushbackReader InputStream OutputStream))
  )

(defmulti format (fn [content-type] content-type))

(defn decode-str [options]
  (reify
    m/Decode
    (decode [_ data charset]
      (slurp (InputStreamReader. ^InputStream data ^String charset)))))

(defn encode-str [options]
  (reify
    m/EncodeToBytes
    (encode-to-bytes [_ data charset]
      (.getBytes data ^String charset))
    m/EncodeToOutputStream
    (encode-to-output-stream [_ data charset]
      (fn [^OutputStream output-stream]
        (.write output-stream (.getBytes data ^String charset))))))

(defmethod format "text/plain"
  [content-type]
  (m/map->Format
   {:name "text/plain"
    :decoder [decode-str]
    :encoder [encode-str]}))

(defmethod format "text/html"
  [content-type]
  (m/map->Format
   {:name "text/html"
    :decoder [decode-str]
    :encoder [encode-str]}))
