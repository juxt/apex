;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.alpha.http.conditional)

(defmulti evaluate-precondition (fn [header provider resource request respond raise] header))

(defn wrap-precondition-evalution
  [provider resource]
  (fn [request respond raise]

    (let [method (:request-method request)]

      ;; See RGC 7232 Section 6 (Precedence)

      ;; TODO: 1. "When recipient is the origin server and If-Match is
      ;; present, evaluate the If-Match precondition"

      ;; TODO: 2. "When recipient is the origin server, If-Match is
      ;; not present, and If-Unmodified-Since is present, evaluate the
      ;; If-Unmodified-Since precondition"

      ;; TODO: 3. "When If-None-Match is present, evaluate the If-None-Match precondition"

      (if-let [if-none-match (get-in request [:headers "if-none-match"])]
        ;; TODO: replace this with evaluation of if-none-match
        (continue)

        ;; "4. When the method is GET or HEAD, If-None-Match is not present, and
        ;; If-Modified-Since is present, evaluate the If-Modified-Since
        ;; precondition, if true, continue to step 5, if false, respond 304 (Not
        ;; Modified)"
        (if (or (= method :get) (= method :head))
          (if-let [if-modified-since (get-in request [:headers "if-modified-since"])]
            (if-not (evaluate-precondition "if-modified-since")
              (respond {:status 304})
              ))


          )
        )))

  ;; TODO: 5. "When the method is GET and both Range and If-Range are present, evaluate the If-Range precondition"

  )
