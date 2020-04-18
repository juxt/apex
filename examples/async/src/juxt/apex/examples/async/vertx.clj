(ns juxt.apex.examples.async.vertx

  (:require
   [integrant.core :as ig]
   [jsonista.core :as jsonista]
   reitit.middleware
   [reitit.ring :as ring]
   reitit.ring.middleware.dev)
  (:import
   (io.vertx.core Handler)
   (io.vertx.core Vertx)
   (io.vertx.core.http HttpServerOptions)))


(defmethod ig/init-key ::vertx
  [_ _]
  (println "Starting Vert.x")
  (Vertx/vertx))

(defmethod ig/halt-key! ::vertx [_ vertx]
  (println "Stopping Vert.x:" vertx)
  (.close vertx)
  (println "Stopped Vert.x"))

(defmethod ig/init-key ::http-server
  [_ {:keys [vertx port] :as opts}]

  (println "Starting Vert.x HttpServer on port" port)

  (let [server (.createHttpServer
                vertx
                (.. (new HttpServerOptions)
                    (setPort port)))]
    (.. server
        (requestHandler
         (reify Handler
           (handle [_ event]
             (doto (.response event)
               (.setChunked true)
               (.setStatusCode 200)
               (.write "Hello World! Clojure and Vert.x!")
               (.end)))))
        listen)))

(defmethod ig/halt-key! ::http-server [_ server]
  (println "Stopping Vert.x HttpServer")
  (.close server)
  (println "Stopped Vert.x HttpServer"))
