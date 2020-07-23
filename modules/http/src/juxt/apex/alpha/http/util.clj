;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.alpha.http.util
  (:require
   [juxt.reap.alpha.regex :as re]
   [juxt.reap.alpha.rfc7231 :as rfc7231])
  (:import
   (java.util Date)))

;; Reap utilities (encode-date, decode-date)

(def ^:private http-date (rfc7231/http-date {}))

(defn ^Date parse-http-date [s]
  (:juxt.http/date
   ((:juxt.reap/decode http-date)
    (re/input s))))

(defn format-http-date [^Date inst]
  (assert (instance? java.util.Date inst) (format "Type is %s" (type inst)))
  ((:juxt.reap/encode http-date)
   {:juxt.http/date inst}))
