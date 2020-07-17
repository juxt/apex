;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.examples.tutorial.util
  (:require
   [juxt.reap.alpha.ring :refer [decode-accept-headers]]
   [juxt.apex.alpha.http.core :as http]
   [juxt.pick.alpha.core :refer [pick]]
   [juxt.pick.alpha.apache :refer [using-apache-algo]]))

;; TODO: Find a location for this function inside a core module (which depends
;; on pick)
(defn pick-variants
  "A convenience wrapper upon pick that resolves variants according to the URIs in
  the :juxt.http/variant-locations entry of the resource."
  [provider resource request]
  (pick
   using-apache-algo
   (conj
    {}
    (decode-accept-headers request)
    [:juxt.http/variants
     (->> (:juxt.http/variant-locations resource)
          (map #(http/lookup-resource provider %)))])))

(defn hexdigest
  "Returns the hex digest of an object. Expects a string as input."
  ([input] (hexdigest input "SHA-256"))
  ([input hash-algo]
   (assert (string? input))
   (let [hash (java.security.MessageDigest/getInstance hash-algo)]
     (. hash update (.getBytes input))
     (let [digest (.digest hash)]
       (apply str (map #(format "%02x" (bit-and % 0xff)) digest))))))
