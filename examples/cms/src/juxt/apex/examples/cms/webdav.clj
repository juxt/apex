;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.examples.cms.webdav
  (:require
   [integrant.core :as ig]
   [juxt.apex.alpha.async.helpers :as a]
   [juxt.apex.alpha.webdav.api :as webdav]))

(defmethod ig/init-key ::router [_ opts]
  (->
   (webdav/make-handler opts)
   ;; This handler requires a Ring-compliant Ring :body request, we
   ;; have a vertx one.
   a/wrap-request-body-as-input-stream))
