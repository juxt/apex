(ns juxt.apex.alpha2.codec
  (:require
   [muuntaja.core :as m]
   [muuntaja.format.core :as mfc]
   [muuntaja.format.json :as mfj]))

(def default-muuntaja
  (m/create
   (->
    m/default-options
    (assoc-in [:formats "application/json"]
              (mfc/map->Format
               {:name "application/json"
                :decoder [mfj/decoder {:decode-key-fn false}]
                :encoder [mfj/encoder]})))))
