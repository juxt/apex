;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.alpha.http.conditional)

(defmulti evaluate-precondition (fn [header provider request respond raise] header))

(defmethod evaluate-precondition "if-modified-since"
  [header provider request respond raise]
  false
  )

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
        (when-let [if-modified-since (get-in request [:headers "if-modified-since"])]
          (when-not (evaluate-precondition "if-modified-since" provider request respond raise)
            (respond {:status 304}))
          (h request respond raise))
        )


      (h request respond raise)))



  )
