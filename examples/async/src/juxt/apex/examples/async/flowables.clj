;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.examples.async.flowables
  (:require
   [juxt.apex.alpha.async.flowable :as f])
  (:import
   (io.vertx.reactivex.core.buffer Buffer)
   (io.reactivex Flowable BackpressureStrategy)))

;; See https://www.baeldung.com/rxjava-2-flowable

;; https://github.com/ReactiveX/RxJava/wiki/Connectable-Observable-Operators

;; Essential reading:
;; http://blog.josephwilk.net/clojure/building-clojure-services-at-scale.html
;; Aleph, Async, HTTP, Clojure - https://gist.github.com/kachayev/9911710758b56477e7423b5bd8dad144
;; https://vorpus.org/blog/notes-on-structured-concurrency-or-go-statement-considered-harmful/#nurseries-a-structured-replacement-for-go-statements

(defn flow-example [_ _ respond _]
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


(defn ticker-example
  "An example demonstrating how to merges together two separate feeds."
  [opts _ respond _]
  (respond
   (let [bus (.. (:vertx opts) eventBus)]
     {:status 200
      :headers {"content-type" "text/event-stream"}
      :body (Flowable/merge
             (for [feed [:juxt-feed :crux-feed]]
               (->>
                (.. bus (consumer (get-in opts [feed :topic])) toFlowable)
                (f/map (comp f/server-sent-event (memfn body))))))})))
