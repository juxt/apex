;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.examples.async.vertx
  (:require
   [integrant.core :as ig]
   reitit.middleware
   [clojure.string :as string]
   reitit.ring.middleware.dev
   [clojure.string :as string])
  (:import
   (io.vertx.core Vertx Handler MultiMap Promise)
   (io.vertx.core.http HttpServer HttpServerOptions)))

(defmethod ig/init-key ::vertx
  [_ _]
  (Vertx/vertx))

(defmethod ig/halt-key! ::vertx
  [_ vertx]
  (.close vertx))

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

(deftype VertxHandler [f]
  Handler
  (handle [_ ev] (f ev)))

(defn ^HttpServer run-http-server
  [handler {:keys [vertx port]}]
  (let [server
        (..
         vertx
         (createHttpServer
          (..
           (new HttpServerOptions)
           (setPort port))))]
    (..
     server
     (requestHandler
      (reify Handler
        (handle [_ req]
          (let [body-buffer (Promise/promise)]
            (.bodyHandler
             req
             (->VertxHandler
              (fn [buffer]
                (println "Receiving buffer!" buffer ",delivering to atom" body-buffer)
                (.complete body-buffer (io.netty.buffer.ByteBufInputStream. (.getByteBuf buffer))))))
            (handler
             (->RingRequestMap
              body-buffer
              {:server-port port
               :server-name (.host req)
               :remote-addr (str (.remoteAddress req))
               :uri (.path req)
               :query-string (.query req)
               :scheme (.scheme req)
               :request-method (keyword (string/lower-case (.rawMethod req)))
               :protocol (str (.version req))
               :headers (->RingHeaders (.headers req))
               })
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
                      (setStatusCode status))]

                 #_(.closeHandler (.connection req)
                                  (->VertxHandler (fn [_]
                                                    (println "connection closed")
                                                    )))
                 #_(println "connection" (.connection req))

                 #_(doseq [[k v] headers]
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

                 (..
                  response
                  (write body)
                  end

                  ;; Add end handlers

                  )))
             (fn [e] (println "ERROR" e))))
          )))
     listen)))

(defn root-handler [req respond raise]
  (let [msg "hi"
        ;;(format "body is %s" (slurp (:body req)))

        #_(format "OK - request is %s" (pr-str req))]

    (.onSuccess (:body req)
                 (new VertxHandler (fn [body]
                                     (println "Body is" body)
                                     (respond
                                      {:status 200
                                       :body (str "Body was" (slurp body))}))))))

(defmethod ig/init-key ::http-server
  [_ opts]
  (run-http-server #'root-handler opts))

(defmethod ig/halt-key! ::http-server [_ server]
  (.close server))


;; Write the following

;; A GET response with a sync string body
;; A GET response with an async body
;; A SSE feed with an async body
;; A POST request with a sync string body
;; A POST request with a async body stream (file upload)
;; A multipart/form-data POST/PUT
;; A GET request for a file
;; Websockets
