;; Copyright © 2019, JUXT LTD.

(ns juxt.apex.alpha.trace.trace)

(defn commit-request-journal [request-history-atom m]
  (swap!
   request-history-atom
   (fn [history]
     (conj history (assoc m :index (count history))))))

(defn make-journal-entry [journal type req middleware]
  (assert journal)
  (assert req)
  (swap!
   journal
   (fn [journal]
     (conj journal
           {:index (count journal)
            :type type
            :apex.trace/middleware middleware
            :apex.trace/request-state (dissoc req :apex/request-journal-atom)}))))

(defn make-exception-entry [type journal e middleware]
  (assert journal)
  (assert e)
  (swap!
   journal
   (fn [journal]
     (conj journal
           {:index (count journal)
            :type type
            :apex.trace/middleware middleware
            :exception e}))))

(defn link-up [reqs]
  (->> reqs
       (partition-all 2 1)
       (map (fn [[x sub]] (cond-> x sub (assoc :apex.trace/next-request-state sub))))
       vec))

(def wrap-trace-outer
  {:name "Outer trace"
   ::trace-middleware true
   :compile
   (fn [route-data router-opts]
     (fn [h request-history-atom]
       (fn
         ([req]
          (let [t0 (new java.util.Date)
                a (atom [{:index 0
                          :type :begin
                          :apex.trace/middleware wrap-trace-outer
                          :apex.trace/request-state req}])]
            (let [response (h (assoc req :apex/request-journal-atom a))
                  t1 (new java.util.Date)]
              (commit-request-journal
               request-history-atom
               {:apex/start-date t0
                :apex/duration (- (.getTime t1) (.getTime t0))
                :apex/request-journal (link-up @a)})
              response)))
         ([req respond raise]
          (let [t0 (new java.util.Date)
                a (atom [{:index 0
                          :type :begin
                          :apex.trace/middleware wrap-trace-outer
                          :apex.trace/request-state req}])]
            (h
             (assoc req :apex/request-journal-atom a)
             (fn [response]
               (let [t1 (new java.util.Date)]
                 (commit-request-journal
                  request-history-atom
                  {:apex/start-date t0
                   :apex/duration (- (.getTime t1) (.getTime t0))
                   :apex/request-journal (link-up @a)}))
               (respond response))
             (fn [e]
               (let [t1 (new java.util.Date)]
                 (commit-request-journal
                  request-history-atom
                  {:apex/start-date t0
                   :apex/duration (- (.getTime t1) (.getTime t0))
                   :apex/request-journal (link-up @a)}))
               ;; TODO: Produce a 500 response?
               ;; This should be subject to OpenAPI - probably handled in a custom middleware
               ;; Therefore, we should never get here

               ;; Trace on every raise on every middleware, so it's
               ;; easy to see how an error gets threaded through each
               ;; middleware
               (raise e))))))))})

(def wrap-trace-inner
  {:name "Inner trace"
   ::trace-middleware true
   :compile
   (fn [route-data router-opts]
     (fn [h request-history-atom]
       (fn
         ([req]
          (let [journal (:apex/request-journal-atom req)]
            (make-journal-entry journal :invoke-handler req wrap-trace-inner)
            (h req)))
         ([req respond raise]
          (let [journal (:apex/request-journal-atom req)]
            (make-journal-entry journal :invoke-handler req wrap-trace-inner)
            (try
              (println "Calling handler:" h)
              (h req respond raise)
              (catch Throwable e
                ;; TODO: Add e to trace
                ;; The handler has not caught and handles its own errors
                (make-exception-entry :exception-from-handler journal e wrap-trace-inner)
                (raise e))))))))})

(defn middleware-proxy
  "Given a reitit.middleware/Middleware record (containing a :wrap entry
  of a single-aray function as per provided by reitit.middleware/into-middleware), update the wrap function ..."
  [middleware req-f]
  (assert middleware)
  (update
   middleware
   :wrap
   (fn [wrap]
     (fn [h]
       ;; Here is the sleight-of-hand where we call wrap with the
       ;; handler to create a new handler we can intercept calls on.
       (let [h (wrap h)]
         (fn
           ([req]
            (h (req-f req)))
           ([req respond raise]
            ;; TODO: Ideally also track any errors that happen in
            ;; respond in case delegate doesn't properly deal with
            ;; them. Test for this.
            (try
              (h (req-f req)
                 respond
                 raise)
              (catch Throwable e
                ;; TODO: Add a note that this has been
                (raise (ex-info "Throwable escaped from asynchronous middleware" {:middleware middleware} e))
                )))))))))

(defn journalling-proxy [middleware]
  (middleware-proxy
   middleware
   (fn [req]
     (let [journal (:apex/request-journal-atom req)]
       (make-journal-entry journal :enter req middleware)
       req))))

(defn trace-middleware-transform [request-history-atom]
  (fn [middleware]
    (vec
     (concat
      ;; Add some per-request container to store 'traces' which can be
      ;; rendered later.
      [[wrap-trace-outer request-history-atom]]
      (vec
       (map
        (fn [middleware]
          (journalling-proxy middleware))
        middleware))
      [[wrap-trace-inner request-history-atom]]
      ))))
