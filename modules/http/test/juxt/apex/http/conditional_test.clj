;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.http.conditional-test
  (:require
   [juxt.apex.alpha.http.conditional :refer [if-modified-since?]]
   [clojure.test :refer [deftest is are]]
   [juxt.apex.alpha.http.core :as http]))

(deftest if-modified-since-test
  (are [this other expected]
      (= expected
         (if-modified-since?
             (http/decode-date this)
             (http/decode-date other)))

      "Wed, 8 Jul 2020 10:20:00 +0100"
      "Wed, 8 Jul 2020 10:00:00 +0100"
      true

      "Wed, 8 Jul 2020 10:00:00 +0100"
      "Wed, 8 Jul 2020 10:20:00 +0100"
      false

      "Wed, 8 Jul 2020 10:00:00 +0100"
      "Wed, 8 Jul 2020 10:00:00 +0100"
      false))
