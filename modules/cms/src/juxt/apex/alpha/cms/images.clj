(ns juxt.apex.alpha.cms.images
  (:require
   [clojure.java.io :as io]))

(defn resize-image [image-inputstream target-width out-stream]
  (let [im
        (javax.imageio.ImageIO/read
         image-inputstream)

        source-width (.getWidth im)

        factor (/ source-width target-width)

        w (/ (.getWidth im) factor)
        h (/ (.getHeight im) factor)

        om (new java.awt.image.BufferedImage w h (.getType im))
        g2d (.createGraphics om)
        ]

    (.drawImage g2d im 0 0 w h nil)
    (.dispose g2d)

    (javax.imageio.ImageIO/write
     om
     "jpg"
     out-stream)))
