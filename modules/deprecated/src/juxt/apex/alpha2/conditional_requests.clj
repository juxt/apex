;; Copyright © 2019, JUXT LTD.

(ns juxt.apex.alpha2.conditional-requests
  (:require
   [juxt.apex.alpha2.util :refer [to-rfc-1123-date-time from-rfc-1123-date-time]]))

(defmulti validator->header (fn [[k v]] k))

(defmethod validator->header :apex/entity-tag
  [[_ v]] ["ETag" (:value v)])

(defmethod validator->header :apex/last-modified
  [[_ v]]
  ["Last-Modified" (to-rfc-1123-date-time (:value v))])

(defn since? [instant since]
  (= (.compareTo instant since) 1))

(def wrap-conditional-request
  {:name "Conditional requests"
   :wrap
   (fn [h validators]
     (fn
       ([req]
        (let [operation (:oas/operation req)
              opId (get operation "operationId")]
          (if validators
            (validators
             req
             (fn [req]
               (let [since (from-rfc-1123-date-time (get-in req [:headers "if-modified-since"]))
                     modified (:value (:apex/last-modified req))]

                 (if (and since modified (not (since? modified since)))
                   {:status 304}
                   (let [response (h req)]
                     (update response :headers merge (into {} (map validator->header (select-keys req [:apex/entity-tag :apex/last-modified]))))))))
             (fn [ex] (throw ex)))
            ;; No validators
            (h req))))
       ([req respond raise]
        (let [operation (:oas/operation req)
              opId (get operation "operationId")]
          (if validators
            (validators
             req
             (fn [req]
               (let [since (from-rfc-1123-date-time (get-in req [:headers "if-modified-since"]))
                     modified (:value (:apex/last-modified req))]

                 (if (and since modified (not (since? modified since)))
                   (respond {:status 304})
                   (h req
                      (fn [response]
                        (respond (update response :headers merge (into {} (map validator->header (select-keys req [:apex/entity-tag :apex/last-modified]))))))
                      raise))))
             raise)
            ;; No validators
            (h req respond raise))))))})
