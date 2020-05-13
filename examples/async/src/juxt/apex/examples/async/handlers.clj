(ns juxt.apex.examples.async.handlers
  (:require
   [org.reactivestreams.flow :as rs]
   [juxt.apex.alpha.async.flowable :as f]
   [juxt.apex.examples.async.async-helpers :refer [h pipe-to-file]])
  (:import
   (io.vertx.reactivex.core.buffer Buffer)
   (io.reactivex Flowable BackpressureStrategy))
  )

(def file-serving-example
  (->
   (fn [req respond raise]

     #_(.. (:apex.vertx/vertx req)
           fileSystem
           (open
            "/home/malcolm/dominic.jpg"
            (new io.vertx.core.file.OpenOptions (new JsonObject {"read" true}))
            (reify Handler
              (handle
                  (fn [ar]

                    )))))

     ;; Send file
     #_(.. vertx
           fileSystem
           (open
            "/home/malcolm/dominic.jpg"
            (new io.vertx.core.file.OpenOptions (new JsonObject {"read" true}))
            (new VertxHandler
                 (fn [result]
                   (if (.succeeded result)
                     (let [file (.result result)]
                       (.pipeTo
                        file response
                        (new VertxHandler
                             (fn [result]
                               (if (.succeeded result)
                                 (.end response))))))
                     (println "Not succeeded: result is" result))))))

     #_(.. vertx
           fileSystem
           (open
            #_"/photos/Personal/ski2020/IMG_20200128_162602.jpg"
            "/home/malcolm/dominic.jpg"
            (new io.vertx.core.file.OpenOptions (new JsonObject {"read" true}))


            (new VertxHandler
                 (fn [ar]
                   (println "ar is" ar)
                   ))))
     ;;(assert vertx)
     (respond
      {:status 200
       :headers {"content-type" "image/jpeg"}
       :body "TODO: Replace this with some file\n"}))))

(defn flow-example [opts req respond raise]
  (respond
   {:status 200
    ;; The body is a subscriber
    ;; "The recommended way of creating custom Flowables is by using the create(FlowableOnSubscribe, BackpressureStrategy) factory method:" -- http://reactivex.io/RxJava/2.x/javadoc/io/reactivex/Flowable.html
    ;;  Flowables support backpressure and require Subscribers to signal demand via Subscription.request(long).

    :body
    (Flowable/create
     (reify io.reactivex.FlowableOnSubscribe
       (subscribe [_ e]
         (.onNext e (Buffer/buffer "Hello\n\n"))
         (.onNext e (Buffer/buffer "Hello\n\n"))
         (.onNext e (Buffer/buffer "Hello\n\n"))
         (Thread/sleep 200)
         (.onNext e (Buffer/buffer "Goodbye\n\n"))
         (.onComplete e)))
     BackpressureStrategy/BUFFER)}))

(defn backpressure-example [opts req respond raise]
  (respond
   {:status 200
    :headers {}

    ;; NOTE: We might often use a subclass of
    ;; java.util.concurrent.SubmissionPublisher for managing
    ;; subscriptions

    :body
    ;; We return a publisher
    (reify rs/Publisher
      (subscribe [_ subscriber]
        (rs/on-subscribe
         subscriber
         (reify rs/Subscription
           (cancel [_]
             (println "bpe: cancelling subscription"))
           (request [_ n]
             (println "bpe: subscriber is requesting" n "items"))))
        (println "bpe: subscribing with subscriber" subscriber)))}))

(defn upload-file-example [opts req respond raise]
  (let [vertx (:apex.vertx/vertx req)
        vertx-request (:apex.vertx/request req)]

    (.setExpectMultipart vertx-request true)

    ;; NOTE: HTTP/2 supports stream reset at any time during the
    ;; request/response -- https://vertx.io/docs/vertx-core/java/ --
    ;; which is great for telling our CMS clients that we already have
    ;; a given file. Test with --http2

    (. vertx-request
       uploadHandler
       (h (fn [upload]
            (pipe-to-file
             vertx
             upload
             (str "COPY3-" (.filename upload))
             {:on-success (fn [file]
                            (respond
                             {:status 200
                              :body (format "Thanks! Bytes received: %s\n" (.getWritePos file))}))
              :on-failure raise}))))))


;; See https://www.baeldung.com/rxjava-2-flowable

;; https://github.com/ReactiveX/RxJava/wiki/Connectable-Observable-Operators

;; Essential reading:
;; http://blog.josephwilk.net/clojure/building-clojure-services-at-scale.html
;; Aleph, Async, HTTP, Clojure - https://gist.github.com/kachayev/9911710758b56477e7423b5bd8dad144
;; https://vorpus.org/blog/notes-on-structured-concurrency-or-go-statement-considered-harmful/#nurseries-a-structured-replacement-for-go-statements


(defn sse-example [req respond raise]
  (respond
   {:status 200
    :headers {"content-type" "text/event-stream"}
    :body (->>
           (f/range 1 10000)
           ;;(f/throttle-first 100 TimeUnit/MILLISECONDS)
           ;;(#(.timestamp %))
           (#(.limit % 50))
           (#(.materialize %))
           ;;           (#(.dematerialize %))
           (f/map f/server-sent-event))}))

(defn ticker-example
  "An example demonstrating how to merges together two separate feeds."
  [opts req respond raise]
  (respond
   (let [bus (.. (:vertx opts) eventBus)]
     {:status 200
      :headers {"content-type" "text/event-stream"}
      :body (Flowable/merge
             (for [feed [:juxt-feed :crux-feed]]
               (->>
                (.. bus (consumer (get-in opts [feed :topic])) toFlowable)
                (f/map (comp f/server-sent-event (memfn body))))))})))
