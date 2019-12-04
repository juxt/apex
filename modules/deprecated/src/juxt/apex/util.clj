(ns juxt.apex.util)

(defn to-rfc-1123-date-time [instant]
  (.format java.time.format.DateTimeFormatter/RFC_1123_DATE_TIME
           (java.time.ZonedDateTime/ofInstant instant (java.time.ZoneId/of "GMT"))))

(defn from-rfc-1123-date-time [s]
  (some->> s
           (.parse java.time.format.DateTimeFormatter/RFC_1123_DATE_TIME)
           (java.time.Instant/from)))

(defn ring-handler
  "Given a function on a request, return a Ring middleware function that
  calls the function on the request."
  ([req-f]
   (ring-handler req-f (fn [response & args] response)))
  ([req-f resp-f]
   (fn [h & args]
     (fn
       ([req] (apply resp-f (h (apply req-f req args)) args))
       ([req respond raise]
        (let [req
              (try
                (apply req-f req args)
                (catch Exception e (raise e)))]
          (h req
             (fn [response]
               (let [response
                     (try
                       (apply resp-f response args)
                       (catch Exception e (raise e)))]
                 (respond response)))
             raise)))))))
