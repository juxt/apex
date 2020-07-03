;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.examples.tutorial.server
  (:require
   [clojure.string :as string]
   [juxt.apex.alpha.http.header-names :refer [header-canonical-case]]
   [org.reactivestreams.flow :as rs]
   [integrant.core :as ig]
   [juxt.apex.alpha.vertx.helpers :refer [h]])
  (:import
   (io.vertx.core MultiMap)
   (io.vertx.reactivex.core.buffer Buffer)
   (io.vertx.core.http HttpServerOptions)
   (io.vertx.reactivex.core Vertx)
   (io.vertx.reactivex.core.http HttpServer)))

(defn ^HttpServer run-http-server
  [router {:keys [vertx port] :as opts}]
  (let [server-options
        (..
         (new HttpServerOptions)
         (setLogActivity (get opts :vertx/log-activity false)))
        server
        (..
         vertx
         (createHttpServer
          (..
           server-options
           (setPort port))))]
    (..
     server
     (requestHandler
      (h
       (fn [req]
         (let [ring-req
               {:server-port port
                :server-name (.getHost server-options)
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
                  (.putHeader response (header-canonical-case k) (str v)))

                ;; Now, make flowable

                ;; If the body is not a String, InputStream etc..


                ;; On Java 9 and above, body may return a
                ;; java.util.concurrent.Flow.Publisher.  Java 8 and below should
                ;; use org.reactivestreams.Publisher:
                ;; http://www.reactive-streams.org/reactive-streams-1.0.3-javadoc/org/reactivestreams/Publisher.html?is-external=true

                ;; Body must satisfy protocol

                (cond

                  (instance? java.io.File body)
                  (. response sendFile (.getAbsolutePath body))

                  (satisfies? rs/Publisher body)
                  (rs/subscribe body (.toSubscriber response))

                  (and body (= (Class/forName "[B") (.getClass body)))
                  (.. response (write (Buffer/buffer body)) end)

                  ;; TODO: Support java.io.InputStream

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

(defmethod ig/init-key ::vertx
  [_ _]
  (Vertx/vertx))

(defmethod ig/halt-key! ::vertx
  [_ vertx]
  (.close vertx))

(defmethod ig/init-key ::http-server
  [_ {:keys [handler] :as opts}]
  (run-http-server handler opts))

(defmethod ig/halt-key! ::http-server [_ server]
  (.close server))
