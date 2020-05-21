;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.examples.cms.webdav
  (:require
   [integrant.core :as ig]
   [juxt.apex.alpha.webdav.api :as webdav]))

(defmethod ig/init-key ::router [_ opts]
  (webdav/make-handler opts))
