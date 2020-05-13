;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.examples.async.rs
  (:require
   [org.reactivestreams.flow :as rs]))

(defn backpressure-example [_ _ respond _]
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
