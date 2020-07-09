;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.alpha.http.conditional
  (:require
   [juxt.apex.alpha.http.core :as http]))

(defn if-modified-since? [this other]
  (.isAfter this other))

(defmulti evaluate-precondition (fn [header provider request respond raise] header))

(defmethod evaluate-precondition "if-modified-since"
  [header provider request respond raise]
  (if (satisfies? http/LastModified provider)
    (let [last-modified (http/last-modified provider (:juxt.http/resource request))
          if-modified-since
          (some-> (get-in request [:headers "if-modified-since"])
                  http/decode-date)]

      (if (and last-modified if-modified-since)
        (if-modified-since? last-modified if-modified-since)
        true))
    ;; Default is to assume there's a modification, we don't know there isn't
    ;; one!
    true))

;; From RFC 7232:
;; "(an) origin server MUST evaluate received request preconditions after it has
;; successfully performed its normal request checks and just before it would
;; perform the action associated with the request method.  A server MUST ignore
;; all received preconditions if its response to the same request without those
;; conditions would have been a status code other than a 2xx (Successful) or 412
;; (Precondition Failed).  In other words, redirects and failures take
;; precedence over the evaluation of preconditions in conditional requests. "

(defn wrap-precondition-evalution
  [h provider]
  (fn [request respond raise]

    (let [method (:request-method request)]

      ;; See RGC 7232 Section 6 (Precedence)

      ;; TODO: 1. "When recipient is the origin server and If-Match is
      ;; present, evaluate the If-Match precondition"

      ;; TODO: 2. "When recipient is the origin server, If-Match is
      ;; not present, and If-Unmodified-Since is present, evaluate the
      ;; If-Unmodified-Since precondition"

      ;; TODO: 3. "When If-None-Match is present, evaluate the If-None-Match precondition"

      (when-let [if-none-match (get-in request [:headers "if-none-match"])]

        ;; TODO: replace this with evaluation of if-none-match
        (h request respond raise))

      ;; "4. When the method is GET or HEAD, If-None-Match is not present, and
      ;; If-Modified-Since is present, evaluate the If-Modified-Since
      ;; precondition, if true, continue to step 5, if false, respond 304 (Not
      ;; Modified)"
      (when (or (= method :get) (= method :head))
        (when (get-in request [:headers "if-modified-since"])
          (let [result (evaluate-precondition "if-modified-since" provider request respond raise)]
            (when-not result
              (respond {:status 304})))
          (h request respond raise)))

      (h request respond raise))))
