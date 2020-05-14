;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.examples.async.vertx
  (:require
   [juxt.apex.examples.async.router :refer [router]]
   [integrant.core :as ig]
   reitit.middleware
   [juxt.apex.examples.async.async-helpers :refer [h]]
   [clojure.string :as string]
   reitit.ring.middleware.dev
   [org.reactivestreams.flow :as rs])
  (:import
   (io.vertx.core MultiMap)
   (io.vertx.core.http HttpServerOptions)
   (io.vertx.reactivex.core Vertx)
   (io.vertx.reactivex.core.http HttpServer)))

;; Adapt org.reactivestreams.Subscription to the Clojure protocol
;; See https://www.reactive-streams.org/reactive-streams-1.0.2-javadoc/org/reactivestreams/Subscription.html
(extend-protocol rs/Subscription
  org.reactivestreams.Subscription
  (cancel [s]
    (println "rs/Subscription org.reactivestreams.Subscription cancel")
    (.cancel s))
  (request [s n]
    (println "rs/Subscription org.reactivestreams.Subscription request" n)
    (.request s n)))

;; Adapt Vert.x subscriber to the Clojure protocol (e.g. the Vert.x HTTP response)
(extend-protocol rs/Subscriber
  io.vertx.reactivex.WriteStreamSubscriber
  (on-complete [s]
    (println "on-complete")
    (.onComplete s))
  (on-error [s t]
    (println "on-error")
    (.onError s t))
  (on-next [s item]
    (println "on-next")
    (.onNext s item))
  (on-subscribe [s subscription]
    (println "on-subscribe: subscription is" subscription)
    (.onSubscribe s (reify org.reactivestreams.Subscription
                      (cancel [_] (rs/cancel subscription))
                      (request [_ n] (rs/request subscription n))))))

;;(clojure.reflect/reflect io.vertx.reactivex.WriteStreamSubscriber)

;;(clojure.reflect/reflect io.reactivex.Flowable)

(deftype RingHeaders [mm]
  clojure.lang.ILookup
  (valAt [_ k] (when-let [vals (.getAll mm (str k))]
                 (string/join "," vals)))
  ;; TODO: Revisit semantics here
  (valAt [this k not-found] (or (.valAt this (str k)) not-found))
  clojure.lang.Seqable
  (seq [this] (seq (into {} (for [n (.names mm)] [(string/lower-case n) (.valAt this n)])))))

(deftype RingRequestMap [p m]
  clojure.lang.ILookup
  (valAt [_ k]
    (case k
      :body p
      (get m k)))
  ;; TODO: Revisit semantics here
  (valAt [this k not-found]
    (case k
      :body :todo-body
      (get m k not-found))))

(defn ^HttpServer run-http-server
  [router {:keys [vertx port] :as opts}]
  (let [options (new HttpServerOptions)
        server
        (..
         vertx
         (createHttpServer
          (..
           options
           (setPort port))))]
    (..
     server
     (requestHandler
      (h
       (fn [req]
         (let [ring-req
               {:server-port port
                :server-name (.getHost options)
                :remote-addr (str (.remoteAddress req))
                :uri (.path req)
                :query-string (.query req)
                :scheme (.scheme req)
                :request-method (keyword (string/lower-case (.rawMethod req)))
                :protocol (str (.version req))
                ;; This is aiming at Ring 2 lower-case request header
                ;; fields
                :headers (into {}
                               (for [[k v] (.entries (.headers req))]
                                 [(string/lower-case k) v]))

                ;; You need access to this for when there is no
                ;; alternative but to use a lower-level Vert.x API,
                ;; such as multipart uploads.
                :apex.vertx/request req ; low-level interface

                :apex.vertx/vertx vertx
                #_(->RingHeaders (.headers req))}]
           (router
            opts
            ring-req
            ;; Respond function
            (fn [{:keys [status headers body]}]
              ;; header-map here is only to allow us to preserve the
              ;; case of response headers, while potentially retrieving
              ;; them. Probably better to keep all as lower-case.
              (let [header-map (cond-> (MultiMap/caseInsensitiveMultiMap)
                                 headers (.. (addAll headers)))
                    content-length (.get header-map "content-length")
                    response
                    (..
                     req
                     response
                     (setChunked (not content-length))
                     ;;(setChunked false) ; while playing with SSE
                     (setStatusCode status))]

                #_(.closeHandler (.connection req)
                                 (->VertxHandler (fn [_]
                                                   (println "connection closed")
                                                   )))
                #_(println "connection" (.connection req))

                (doseq [[k v] headers]
                  ;; v can be a String, or Iterable<String>
                  (.putHeader response k (str v)))

                ;; Now, make flowable

                ;; If the body is not a String, InputStream etc..


                ;; On Java 9 and above, body may return a
                ;; java.util.concurrent.Flow.Publisher.  Java 8 and below should
                ;; use org.reactivestreams.Publisher:
                ;; http://www.reactive-streams.org/reactive-streams-1.0.3-javadoc/org/reactivestreams/Publisher.html?is-external=true

                ;; Body must satisfy protocol

                (cond

                  (satisfies? rs/Publisher body)
                  (rs/subscribe body (.toSubscriber response))

                  :else
                  (cond-> response
                    body (.write body)
                    true (.end)
                    ))))

            (fn [e]
              (println "ERROR: " e)
              (..
               req
               response
               (setStatusCode 500)
               (setChunked true)
               (write "ERROR\n")
               (end))))))))

     listen)))


(defn body-input-stream-handler [req respond _]
  ;; This demonstrates the use of calling .bodyHandler on the request.
  ;; body-buffer here is a Vertx Promise (let [body-buffer (Promise/promise)])
  #_(.bodyHandler
     req
     (->VertxHandler
      (fn [buffer]
        (println "Receiving buffer!" buffer ",delivering to atom" body-buffer)
        (.complete body-buffer (io.netty.buffer.ByteBufInputStream. (.getByteBuf buffer))))))
  (.onSuccess
   (:body req)
   (h (fn [body]
        (println "Body is" body)
        (respond
         {:status 200
          :body (str "Body was" (slurp body))})))))

(defmethod ig/init-key ::vertx
  [_ _]
  (Vertx/vertx))

(defmethod ig/halt-key! ::vertx
  [_ vertx]
  (.close vertx))

;; See
;; https://github.com/vert-x3/vertx-examples/blob/master/rxjava-2-examples/src/main/java/io/vertx/example/reactivex/web/backpressure/Server.java
;; for generating 503 as back-pressure on requests.

(defmethod ig/init-key ::http-server
  [_ {:keys [router] :as opts}]
  (run-http-server router opts))

(defmethod ig/halt-key! ::http-server [_ server]
  (.close server))

;; Write the following:

;; - [X] A SSE feed with an async body
;; - [ ] A GET request for a file (streaming a large photo)
;; - [ ] A GET response with an async body
;; - [ ] Caching middleware
;; - [ ] Charset conversion middleware - BAD IDEA - no good way of being sure what the handler produces (or consumes)
;; - [ ] Language translation middleware
;; - [ ] Gzip encoding example (using Vertx compression level - see io.vertx.core.http.HttpServerOptions)
;; - [ ] A GET response with a sync string body
;; - [ ] A POST request with a sync string body
;; - [X] A POST request with a async body stream (file upload)
;; - [X] A multipart/form-data POST/PUT
;; - [ ] Websockets

;; Backpressure


#_(reflect io.reactivex.functions.Consumer)

#_(reflect io.vertx.reactivex.WriteStreamSubscriber)
