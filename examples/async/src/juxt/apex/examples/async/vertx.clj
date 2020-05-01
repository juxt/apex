;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.examples.async.vertx
  (:require
   [clojure.reflect :refer [reflect]]
   [integrant.core :as ig]
   [jsonista.core :as jsonista]
   reitit.middleware
   [clojure.string :as string]
   reitit.ring.middleware.dev
   [clojure.string :as string])
  (:import
   (io.vertx.core Handler MultiMap Promise)
   (io.vertx.core.http HttpServerOptions)
   (io.vertx.core.json JsonObject)
   (io.vertx.reactivex.core Vertx)
   (io.vertx.reactivex.core.http HttpServer)
   (io.reactivex Flowable)
   (java.util.concurrent TimeUnit)
   ))

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
  [handler {:keys [vertx port]}]
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
      (reify Handler
        (handle [_ req]
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
                 :apex.vertx/vertx vertx
                 #_(->RingHeaders (.headers req))}]
            (handler
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

                 #_(.setPeriodic
                    vertx 1000
                    (->VertxHandler
                     (fn [i]
                       (println "Timed event" (.ended response))
                       (if (.ended response)
                         (.cancelTimer vertx i)
                         (.write response "event!\r\n\r\n"))

                       )))

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

                 ;; Now, make flowable

                 ;; If the body is not a String, InputStream etc..


                 (if (clojure.core/instance? io.reactivex.Flowable body)


                   (let [subscriber (.toSubscriber response)]
                     (.onWriteStreamError
                      subscriber
                      (reify io.reactivex.functions.Consumer
                        (accept [_ obj]
                          (println "on-write-stream-error")
                          )))

                     (.onWriteStreamEnd
                      subscriber
                      (reify io.reactivex.functions.Action
                        (run [_]
                          (println "on-write-stream-end")
                          )))

                     (.onWriteStreamEndError
                      subscriber
                      (reify io.reactivex.functions.Consumer
                        (accept [_ obj]
                          (println "on-write-stream-end-error")
                          )))

                     (.onError
                      subscriber
                      (reify io.reactivex.functions.Consumer
                        (accept [_ obj]
                          (println "on-error: " obj)
                          )))

                     (.onComplete
                      subscriber
                      (reify io.reactivex.functions.Action
                        (run [_]
                          (println "on-complete")
                          )))

                     (println "type of subscriber is" subscriber)


                     (.. vertx
                         eventBus
                         (consumer "news-feed")
                         toFlowable
                         (map (reify io.reactivex.functions.Function
                                (apply [_ t]
                                  (io.vertx.reactivex.core.buffer.Buffer/buffer (format "data: %s\r\n\r\n" (.body t))))))
                         (subscribe
                          subscriber
                          #_(reify org.reactivestreams.Subscriber
                              (onComplete [_] (println "on-complete"))
                              (onError [_ t] (println "on-error" t))
                              (onNext [_ ev] (println "on-next" ev))
                              (onSubscribe [_ s] (println "on-subscribe, s is" s)))
                          ;; onNext?

                          #_(.toSubscriber response))))
                   #_(cond-> response
                       body (.write "TODO")
                       true (.end)
                       )

                   (cond-> response
                     body (.write body)
                     true (.end)
                     ))))

             (fn [e] (println "ERROR" e))))
          )))
     listen)))


(defn body-input-stream-handler [req respond raise]
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
   (reify Handler
     (handle [_ body]
       (println "Body is" body)
       (respond
        {:status 200
         :body (str "Body was" (slurp body))})))))

;; toSubscriber (back pressure)
;; toObserver (no back pressure)

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

(defn wrap-cache [handler opts]
  (fn [req respond raise]
    (handler
     (update req :headers conj ["cache-middleware" "yes"])
     respond raise)))

(def cache-example
  (->
   (fn [req respond raise]
     (println "cache-middleware header?" (get-in req [:headers "cache-middleware"]))
     (respond {:status 200
               :headers {"example" "cache-example"}
               :body "TODO: Replace this with some stream\n"}))
   (wrap-cache {})))

;; See https://www.baeldung.com/rxjava-2-flowable

;; https://github.com/ReactiveX/RxJava/wiki/Connectable-Observable-Operators

;; Essential reading:
;; http://blog.josephwilk.net/clojure/building-clojure-services-at-scale.html
;; Aleph, Async, HTTP, Clojure - https://gist.github.com/kachayev/9911710758b56477e7423b5bd8dad144
;; https://vorpus.org/blog/notes-on-structured-concurrency-or-go-statement-considered-harmful/#nurseries-a-structured-replacement-for-go-statements


(defn sse-example [req respond raise]

  ;; Create a RxJava flowable
  (respond
   {:status 200
    :headers {"content-type" "text/event-stream"}
    ;; On Java 9 and above, body may return a
    ;; java.util.concurrent.Flow.Publisher.  Java 8 and below should
    ;; use org.reactivestreams.Publisher:
    ;; http://www.reactive-streams.org/reactive-streams-1.0.3-javadoc/org/reactivestreams/Publisher.html?is-external=true
    :body #_(.publish (Flowable/range 1 1000000))
    (Flowable/range 1 1000000)})
  )



(defn router [req respond raise]
  (condp re-matches (:uri req)

     #"/file.jpg"
    (file-serving-example req respond raise)

    #"/cache-example"
    (cache-example req respond raise)

    ;; SSE
    #"/sse" (sse-example req respond raise)

    #"/debug"
    (respond
     {:status 200
      :headers {"foo" "bar"}
      :body (str
             (jsonista/write-value-as-string
              {"message" "Hello World!"
               "request" req
               "body-info" {"type" (type (:body req))}
               ;;"body" (slurp (:body req))
               })
             "\r\n")})

    (respond {:status 404})

    ))

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
  [_ opts]
  (run-http-server #'router opts))

(defmethod ig/halt-key! ::http-server [_ server]
  (.close server))

(defmethod ig/init-key ::event-publisher
  [_ {:keys [vertx]}]
  (let [eb (.eventBus vertx)]
    {:vertx vertx
     :timer-id (.setPeriodic
                vertx 500
                (reify Handler (handle [_ ev]
                                 #_(println "Publishing news!")
                                 (.publish eb "news-feed" "Some news!"))))}))

(defmethod ig/halt-key! ::event-publisher [_ {:keys [vertx timer-id]}]
  (println "cancelling timer" timer-id)
  (.cancelTimer vertx timer-id))


;; Write the following:

;; Caching middleware
;; Charset conversion middleware - BAD IDEA - no good way of being sure what the handler produces (or consumes)
;; Language translation middleware
;; Gzip encoding example (using Vertx compression level - see io.vertx.core.http.HttpServerOptions)
;; A GET response with a sync string body
;; A GET response with an async body
;; A SSE feed with an async body
;; A POST request with a sync string body
;; A POST request with a async body stream (file upload)
;; A multipart/form-data POST/PUT
;; A GET request for a file
;; Websockets


;;(.exists (java.io.File. "/photos/Personal/ski2020/IMG_20200128_162602.jpg"))

#_(.. (io.vertx.core.Vertx/vertx)
      fileSystem
      (open
       #_"/photos/Personal/ski2020/IMG_20200128_162602.jpg"
       "/home/malcolm/dominic.jpg"
       (new io.vertx.core.file.OpenOptions (new JsonObject {"read" true}))
       (new VertxHandler
            (fn [ar]
              (println "ar is" ar)
              ))))


#_(reflect io.reactivex.functions.Consumer)

#_(reflect io.vertx.reactivex.WriteStreamSubscriber)
