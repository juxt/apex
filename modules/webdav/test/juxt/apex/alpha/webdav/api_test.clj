;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.alpha.webdav.api-test
  (:require
   [juxt.apex.alpha.webdav.api :as webdav]
   [clojure.test :refer [deftest is are testing]]))

(deftest propfind-test
  (let [req
        {:headers
         {"depth" "1",
          "content-type" "application/xml"}
         :body
         "<propfind xmlns=\"DAV:\">
         <prop>
          <resourcetype/>
          <getcontentlength/>
          <getetag/>
          <getlastmodified/>
         </prop>
        </propfind>"
         :uri "/dav/"
         :request-method :propfind}]
    (let [handler (webdav/make-handler {})
          response (handler req)]
      (is (= 200 (:status response)))
      (is (string? (:body response)))
      ;; Check xml structure of response body
      )
    ))
