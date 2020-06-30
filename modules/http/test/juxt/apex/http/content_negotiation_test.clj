;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.http.content-negotiation-test
  (:require
   [clojure.test :refer [deftest is are]]
   [juxt.apex.alpha.http.content-negotiation
    :refer [match-parameters? acceptable-content-type-rating
            acceptable-charset-rating
            assign-language-quality basic-language-match?
            acceptable-encoding-qvalue assign-encoding-quality
            select-most-acceptable-representation]]
   [juxt.reap.alpha.api :as reap]
   [ring.mock.request :refer [request]]))

;; TODO: test for content-type-match?

;; TODO: Refactor and polish these tests so they are consistent with each other. Try to write this tests in a way that references each part of Section 5.3

;; TODO: Test for nils, blank strings, negative qvalues, malformed strings -
;; when and how should a 400 be signalled?

(deftest match-parameters-test
  (is (match-parameters? nil nil))
  (is (match-parameters? {} {}))
  (is (match-parameters? {} {"level" "1" "foo" "bar"}))
  (is (match-parameters? {"LEVEL" "1"} {"level" "1" "foo" "bar"}))
  (is (not (match-parameters? {"LEVEL" "1"} {"level" "2" "foo" "bar"})))
  (is (match-parameters? {"LEVEL" "1" "foo" "bar"} {"level" "1" "foo" "bar"}))
  (is (not (match-parameters? {"LEVEL" "1" "foo" "bar"} {"level" "1" "foo" "baz"}))))

(deftest acceptable-content-type-rating-test
  (are [content-type expected]
      (= expected
         (select-keys
          (acceptable-content-type-rating
           (reap/accept "text/html;q=0.1,text/html;level=2;q=0.4,text/html;LEVEL=3;q=0.5,text/*;q=0.02,*/*;q=0.01")
           (reap/content-type content-type))
          [:qvalue :precedence]))
      "application/json" {:precedence 1 :qvalue 0.01}
      "text/html" {:precedence 3 :qvalue 0.1}
      "text/HTML" {:precedence 3 :qvalue 0.1}
      "text/plain" {:precedence 2 :qvalue 0.02}
      "TEXT/PLAIN" {:precedence 2 :qvalue 0.02}
      "Text/plain" {:precedence 2 :qvalue 0.02}
      "TEXT/HTML" {:precedence 3 :qvalue 0.1} ; case-insensitive
      "text/html;charset=utf-8" {:precedence 3 :qvalue 0.1}
      "text/html;level=2;charset=utf-8" {:precedence 4 :qvalue 0.4}
      "text/html;LEVEL=2;charset=utf-8" {:precedence 4 :qvalue 0.4}
      "text/html;level=3;charset=utf-8" {:precedence 4 :qvalue 0.5}
      "text/html;LEVEL=3;charset=utf-8" {:precedence 4 :qvalue 0.5}))

;; This test represents the table in RFC 7231 Section 5.3.2, where quality
;; values are determined from matching a variant's content-type according to
;; rules of precedence. These rules are specified in the RFC and are independent
;; of the actual content negotiation algorithm that is used.
(deftest acceptable-content-type-qvalue-test

  (let [accepts
        (reap/accept "text/*;q=0.3, text/html;q=0.7, text/html;level=1, text/html;level=2;q=0.4, */*;q=0.5")]

    (are [content-type expected]
        (= expected
           (:qvalue
            (acceptable-content-type-rating
             accepts
             (reap/content-type content-type))))

        "text/html;level=1" 1.0
        "text/html" 0.7
        "text/plain" 0.3
        "image/jpeg" 0.5
        "text/html;level=2" 0.4
        "text/html;level=3" 0.7)))

;; RFC 7231 Section 5.3.5:
;; For example,

;;      Accept-Language: da, en-gb;q=0.8, en;q=0.7

;;    would mean: "I prefer Danish, but will accept British English and
;;    other types of English".

(deftest accept-test

  (are [accept-header expected-content]
      (= expected-content
         (:id
          (select-most-acceptable-representation
           (-> (request :get "/hello")
               (update
                :headers conj
                ["accept" accept-header]))

           [{:id :html
             :juxt.http/content "<h1>Hello World!</h1>"
             :juxt.http/content-type "text/html;charset=utf-8"}

            {:id :html-level-2
             :juxt.http/content "<h1>Hello World!</h1>"
             :juxt.http/content-type "text/html;level=2;charset=utf-8"}

            {:id :plain-text
             :juxt.http/content "Hello World!"
             :juxt.http/content-type "text/plain;charset=utf-8"}])))

    "text/html" :html
    "TEXT/HTML" :html

    "text/html;q=0.8,text/plain;q=0.7" :html
    "text/plain" :plain-text
    "text/html;q=0.8,text/plain" :plain-text

    "TEXT/HTML;level=2;text/html;q=0.8" :html-level-2))

;; TODO: Test quality-of-source

;; See RFC 7231 Section 5.3.3: Accept-Charset

(deftest acceptable-charset-rating-test
  (are [accept-charset content-type expected]
      (= expected
         (->
          (acceptable-charset-rating
           (reap/accept-charset accept-charset)
           (reap/content-type content-type))
          (select-keys [:qvalue :precedence])))

      "iso-8859-5, unicode-1-1;q=0.8"
      "text/plain;charset=iso-8859-5"
      {:qvalue 1.0 :precedence 2}

      "iso-8859-5, unicode-1-1;q=0.8"
      "text/plain;charset=unicode-1-1"
      {:qvalue 0.8 :precedence 2}

      "iso-8859-5, unicode-1-1;q=0.8"
      "text/plain;charset=utf-8"
      {:qvalue 0.0 :precedence 0}

      "iso-8859-5, unicode-1-1;q=0.8,*"
      "text/plain;charset=utf-8"
      {:qvalue 1.0 :precedence 1}))

;; TODO: API should be:-

{:variants []
 :explain? false
 }

;; Result should be

{:variants [] ; an ordered list of variants, callers can choose the first 1
              ; acceptable, the first N acceptable (300) or an unacceptable.
 :vary [] ; list of headers that were could influence the selection
 :explain {} ; an explain, if requested (could be embedded in each of the variants)
 }

;; See RFC 7231 Section 5.3.4: Accept-Encoding

(deftest acceptable-encoding-qvalue-test
  (are [accept-encoding content-encoding expected-qvalue]
      (= (Math/rint (* 1000 expected-qvalue))
         (Math/rint (* 1000 (acceptable-encoding-qvalue
                             (reap/accept-encoding accept-encoding)
                             (reap/content-encoding content-encoding)))))
    "gzip" "gzip" 1.0
    "gzip;q=0.8" "gzip" 0.8
    "gzip" "deflate" 0.0
    "gzip,deflate" "gzip,deflate" 1.0
    "gzip;q=0.8,deflate;q=0.5,*" "identity" 1.0
    "gzip;q=0.8,deflate;q=0.5,*;q=0.1" "identity" 0.1

    ;; Multiple codings applied to content, if all are acceptable, we
    ;; determine the total qvalue with multiplication.
    "gzip" "gzip,deflate" 0.0
    "deflate" "gzip,deflate" 0.0
    "gzip;q=0.9,deflate;q=0.5;compress;q=0.2" "gzip,deflate" 0.45
    "gzip;q=0.4,deflate;q=0.5,compress;q=0.2" "gzip,deflate,compress" 0.04))

;; TODO: Quality factors for encodings ("qs" parameter)

(deftest assign-encoding-quality-test
  (let [variants
        [{:id :gzip
          :juxt.http/content-encoding "gzip"}

         {:id :deflate
          :juxt.http/content-encoding "deflate"}

         {:id :gzip-then-deflate
          :juxt.http/content-encoding "gzip,deflate"}

         {:id :identity
          :juxt.http/content-encoding "identity"}

         ;; :juxt.http/content-encoding defaults to 'identity'
         {:id :unspecified}]]

    (are [accept-encoding-header expected]
        (=
         expected
         (map
          (juxt :id :juxt.http.content-negotiation/encoding-qvalue)
          (sequence
           (assign-encoding-quality accept-encoding-header)
           variants)))

      ;; Rule 1: "If no Accept-Encoding field is in the request, any content-coding is
      ;; considered acceptable by the user agent."
        nil [[:gzip 1.0]
             [:deflate 1.0]
             [:gzip-then-deflate 1.0]
             [:identity 1.0]
             [:unspecified 1.0]]

        "gzip" [[:gzip 1.0]
                [:deflate 0.0]
                [:gzip-then-deflate 0.0]
                [:identity 1.0]

                ;; Rule 2: "If the representation has no content-coding, then it
                ;; is acceptable by default unless specifically excluded by the
                ;; Accept-Encoding field stating either 'identity;q=0' or '*;q=0'
                ;; without a more specific entry for 'identity'."
                [:unspecified 1.0]]

        "deflate" [[:gzip 0.0]
                   [:deflate 1.0]
                   [:gzip-then-deflate 0.0]
                   [:identity 1.0]
                   [:unspecified 1.0]]

        "gzip,deflate;q=0.0" [[:gzip 1.0]
                              [:deflate 0.0]
                              [:gzip-then-deflate 0.0]
                              [:identity 1.0]
                              [:unspecified 1.0]]

        ;; "The asterisk '*' symbol in an Accept-Encoding field matches any available
        ;; content-coding not explicitly listed in the header field."
        "*" [[:gzip 1.0]
             [:deflate 1.0]
             [:gzip-then-deflate 1.0]
             [:identity 1.0]
             [:unspecified 1.0]]

        "gzip;q=0.5,*" [[:gzip 0.5]
                        [:deflate 1.0]
                        [:gzip-then-deflate 0.5]
                        [:identity 1.0]
                        [:unspecified 1.0]]

        "*,gzip;q=0.5" [[:gzip 0.5]
                        [:deflate 1.0]
                        [:gzip-then-deflate 0.5]
                        [:identity 1.0]
                        [:unspecified 1.0]]

        "gzip;q=0.5,*" [[:gzip 0.5]
                        [:deflate 1.0]
                        [:gzip-then-deflate 0.5]
                        [:identity 1.0]
                        [:unspecified 1.0]]

        "gzip;q=0.5,*;q=0.2" [[:gzip 0.5]
                              [:deflate 0.2]
                              [:gzip-then-deflate 0.1]
                              [:identity 0.2]
                              [:unspecified 0.2]]

        "deflate,gzip" [[:gzip 1.0]
                        [:deflate 1.0]
                        [:gzip-then-deflate 1.0]
                        [:identity 1.0]
                        [:unspecified 1.0]]

        "deflate;q=0.5,gzip;q=0.2"
        [[:gzip 0.2]
         [:deflate 0.5]
         [:gzip-then-deflate 0.1]
         [:identity 1.0]
         [:unspecified 1.0]]

        "gzip,identity;q=0.8" [[:gzip 1.0]
                               [:deflate 0.0]
                               [:gzip-then-deflate 0.0]
                               [:identity 0.8]
                               [:unspecified 0.8]]

        "gzip,identity;q=0" [[:gzip 1.0]
                             [:deflate 0.0]
                             [:gzip-then-deflate 0.0]
                             [:identity 0.0]
                             [:unspecified 0.0]]

        "gzip,*;q=0" [[:gzip 1.0]
                      [:deflate 0.0]
                      [:gzip-then-deflate 0.0]
                      [:identity 0.0]
                      [:unspecified 0.0]])))


(deftest accept-encoding-test
  (are [accept-encoding-header variants expected-id]
      (=
       expected-id
       (:id (select-most-acceptable-representation
             (cond-> (request :get "/hello")
               accept-encoding-header
               (update
                :headers conj
                ["accept-encoding" accept-encoding-header]))
             variants)))

      "gzip"
      [{:id :gzip
        :juxt.http/content-encoding "gzip"}]
      :gzip

      "deflate"
      [{:id :deflate
        :juxt.http/content-encoding "deflate"}]
      :deflate

      "gzip;q=0.8,deflate"
      [{:id :deflate
        :juxt.http/content-encoding "deflate"}
       {:id :gzip
        :juxt.http/content-encoding "gzip"}]
      :deflate

      ;; Pick first acceptable variant as per variant order, rather than
      ;; accept-encoding header order.
      "gzip,deflate"
      [{:id :deflate
        :juxt.http/content-encoding "deflate"}
       {:id :gzip
        :juxt.http/content-encoding "gzip"}]
      :deflate

      "gzip,deflate"
      [{:id :gzip-then-deflate
        :juxt.http/content-encoding "gzip,deflate"}]
      :gzip-then-deflate

      "gzip"
      [{:id :gzip-then-deflate
        :juxt.http/content-encoding "gzip,deflate"}
       {:id :identity}]
      :identity

      ;; "If an Accept-Encoding header field is present in a request and none of
      ;; the available representations for the response have a content-coding
      ;; that is listed as acceptable, the origin server SHOULD send a response
      ;; without any content-coding." -- RFC 7231 Section 5.3.4
      "br,compress"
      [{:id :identity}
       {:id :gzip :juxt.http/content-encoding "gzip"}]
      :identity))

;; See RFC 7231 Section 5.3.5: Accept-Language

;; This test represents the example in RFC 4647 Section 3.3.1.
(deftest basic-language-match-test
  (is
   (basic-language-match?
    (:juxt.http/language-range (first (reap/accept-language "en")))
    (:juxt.http/langtag (first (reap/content-language "en")))))

  (is
   (basic-language-match?
    (:juxt.http/language-range (first (reap/accept-language "de-de")))
    (:juxt.http/langtag (first (reap/content-language "de-DE-1996")))))

  (is
   (not
    (basic-language-match?
     (:juxt.http/language-range (first (reap/accept-language "de-de")))
     (:juxt.http/langtag (first (reap/content-language "de-Latn-DE"))))))

  (is
   (not
    (basic-language-match?
     (:juxt.http/language-range (first (reap/accept-language "en-gb")))
     (:juxt.http/langtag (first (reap/content-language "en"))))))

  (is
   (basic-language-match?
    (:juxt.http/language-range (first (reap/accept-language "*")))
    (:juxt.http/langtag (first (reap/content-language "de"))))))

(deftest accept-language-test
  (let [variants
        [{:id :en
          :juxt.http/content "Hello!"
          :juxt.http/content-language "en"}

         {:id :en-us
          :juxt.http/content-language "en-US"
          ;; https://en.wikipedia.org/wiki/Howdy
          ;; Not everyone in the US uses 'Howdy!' but this is just a test...
          :juxt.http/content "Howdy!"}


         {:id :ar-eg
          :juxt.http/content-language "ar-eg,en"
          :juxt.http/content "ألسّلام عليكم"}

         ;; TODO: Test for when no content-language is specified - what should
         ;; we default to?
         ]]

    (are [accept-header expected]
        (=
         expected
         (map
          (juxt :id :juxt.http.content-negotiation/language-qvalue)
          (sequence
           (assign-language-quality accept-header)
           variants)))

      "en" [[:en 1.0][:en-us 1.0][:ar-eg 0.0]]
      "en-us" [[:en 0.0][:en-us 1.0][:ar-eg 0.0]]
      "ar-eg" [[:en 0.0][:en-us 0.0][:ar-eg 1.0]]
      "en-us,en;q=0.8,ar-eg;q=0.2" [[:en 0.8][:en-us 1.0][:ar-eg 0.2]]
      "*" [[:en 1.0][:en-us 1.0][:ar-eg 1.0]]
      "en-us,*;q=0.1" [[:en 0.1][:en-us 1.0][:ar-eg 0.1]])


    (are [accept-language-header expected-greeting]
        (= expected-greeting
           (:juxt.http/content
            (select-most-acceptable-representation
             (cond-> (request :get "/hello")
               accept-language-header
               (update
                :headers conj
                ["accept-language" accept-language-header]))
             variants)))
      "en" "Hello!"
      "en-us" "Howdy!"
      "ar-eg" "ألسّلام عليكم"
      "en-us,en;q=0.8,ar-eg;q=0.2" "Howdy!"
      "*" "Hello!"
      "en-us,*;q=0.1" "Howdy!"
      ;; No rules of precedence apply to languages. If a '*' has greater
      ;; qvalue than another more specific language, it is still
      ;; selected. Hence, en and ar-eg are preferred over en-us, and en is
      ;; selected because it comes before ar-eg.
      "en-us;q=0.8,*" "Hello!"

      nil "Hello!")

    ;; If no Accept-Language header, just pick the first variant.
    (is (= "Hello!"
           (:juxt.http/content
            (select-most-acceptable-representation
             (-> (request :get "/hello"))
             variants))))

    ;; The language quality factor of a variant, if present, is used in
    ;; preference to an Accept-Language header.
    (is
     (=
      "Bonjour!"
      (:juxt.http/content
       (select-most-acceptable-representation
        (-> (request :get "/hello")
            (update
             :headers conj
             ["accept-language" "en"]))
        (conj
         variants
         {:id :fr-fr
          :juxt.http/content "Bonjour!"
          :juxt.http/content-language "fr-FR"
          :juxt.http/language-quality-factor 2})))))))
