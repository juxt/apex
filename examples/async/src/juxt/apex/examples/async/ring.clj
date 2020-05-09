;; TODO: Promote
(ns juxt.apex.examples.async.ring)

;; Modelled on interfaces enclosed by JDK 9's java.util.concurrent.Flow class

(defprotocol Subscription
  (cancel [_] "Causes the Subscriber to (eventually) stop receiving messages.")
  (request [_ n] "Adds the given number n of items to the current unfulfilled demand for this subscription."))

(defprotocol Subscriber
  (on-complete [_] "Method invoked when it is known that no additional
  Subscriber method invocations will occur for a Subscription that is
  not already terminated by error, after which no other Subscriber
  methods are invoked by the Subscription.")

  (on-error [_ t] "Method invoked upon an unrecoverable error
  encountered by a Publisher or Subscription, after which no other
  Subscriber methods are invoked by the Subscription.")

  (on-next [_ item] "Method invoked with a Subscription's next item.")

  (on-subscribe [_ subscription] "Method invoked prior to invoking any
  other Subscriber methods for the given Subscription.")  )

(defprotocol Publisher
  (subscribe [_ subscriber] "Adds the given Subscriber if possible. If
  already subscribed, or the attempt to subscribe fails due to policy
  violations or errors, the Subscriber's on-error method is invoked
  with an IllegalStateException. Otherwise, the Subscriber's
  onSubscribe method is invoked with a new
  Flow.Subscription. Subscribers may enable receiving items by
  invoking the request method of this Subscription, and may
  unsubscribe by invoking its cancel method."))
