;; Copyright © 2019, JUXT LTD.

(ns juxt.apex.alpha2.util
  (:require
   [juxt.jinx-alpha.resolve :refer [resolve-ref]]))

(defn to-rfc-1123-date-time [instant]
  (.format java.time.format.DateTimeFormatter/RFC_1123_DATE_TIME
           (java.time.ZonedDateTime/ofInstant instant (java.time.ZoneId/of "GMT"))))

(defn from-rfc-1123-date-time [s]
  (some->> s
           (.parse java.time.format.DateTimeFormatter/RFC_1123_DATE_TIME)
           (java.time.Instant/from)))

(defn resolve-json-ref
  "Resolve a json reference. If ref is already resolved, then return it
  unchanged."
  [ref {:keys [base-document]}]
  (if (contains? ref "$ref")
    (resolve-ref ref base-document {})
    [ref nil]))
