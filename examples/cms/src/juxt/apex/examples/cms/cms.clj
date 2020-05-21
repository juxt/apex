;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.examples.cms.cms
  (:require
   [juxt.apex.alpha.cms.core :as cms]
   [integrant.core :as ig]))

(defmethod ig/init-key ::router [_ opts]
  (cms/make-router opts))
