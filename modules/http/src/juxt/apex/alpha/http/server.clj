;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.alpha.http.server)

(defprotocol ServerOptions
  :extend-via-metadata true
  (server-header [_]
    "Return the value for server header, or nil to avoid setting it.")
  (server-options [_]))

(defprotocol RequestBody
  (request-body-as-stream [_ request cb]
    "Calls the cb function with the bytes of the request body as a
    byte-stream. A Ring-compatible server should call the cb function with
    the :body entry of the given request."))
