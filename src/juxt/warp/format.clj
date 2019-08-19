(ns juxt.warp.format
  (:refer-clojure :exclude [format])
  (:require
   [muuntaja.format.core :as m])
  (:import (java.io InputStreamReader PushbackReader InputStream OutputStream))
  )

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

(defn text-format [content-type]
  (m/map->Format
   {:name content-type
    :decoder [decode-str]
    :encoder [encode-str]}))
