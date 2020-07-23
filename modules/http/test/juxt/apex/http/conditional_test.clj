;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.http.conditional-test
  (:require
   [juxt.apex.alpha.http.conditional :refer [if-modified-since?]]
   [clojure.test :refer [deftest is are]]
   [juxt.apex.alpha.http.core :as http]
   [juxt.apex.alpha.http.util :as util]))

(deftest if-modified-since-test
  (are [this other expected]
      (= expected
         (if-modified-since?
           (util/parse-http-date this)
           (util/parse-http-date other)))

      "Wed, 08 Jul 2020 10:20:00 GMT"
      "Wed, 08 Jul 2020 10:00:00 GMT"
      true

      "Wed, 08 Jul 2020 10:00:00 GMT"
      "Wed, 08 Jul 2020 10:20:00 GMT"
      false

      "Wed, 08 Jul 2020 10:00:00 GMT"
      "Wed, 08 Jul 2020 10:00:00 GMT"
      false))
