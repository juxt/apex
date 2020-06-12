;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.http.content-negotiation-test
  (:require
   [clojure.test :refer [deftest is are]]
   [juxt.apex.alpha.http.content-negotiation
    :refer [acceptable-content-type-rating
            acceptable-language-rating
            assign-language-quality
            language-match?
            select-most-acceptable-representation]]
   [juxt.reap.alpha.api :as reap]
   [ring.mock.request :refer [request]]
   [juxt.reap.alpha.rfc7231 :as rfc7231]
   [juxt.reap.alpha.regex :as re]))

;; TODO: test for content-type-match?

(deftest acceptable-content-type-rating-test
  (are [content-type expected]
      (= (select-keys
          (acceptable-content-type-rating
           (reap/accept "text/html;q=0.1,text/html;level=2;q=0.4,text/html;level=3;q=0.5")
           (reap/content-type content-type))
          [:qvalue :precedence])
         expected)
    "text/html;charset=utf-8" {:precedence 3 :qvalue 0.1}
    "text/html;level=2;charset=utf-8" {:precedence 4 :qvalue 0.4}))

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

;; This test represents the example in RFC 4647 Section 3.3.1.
(deftest language-match-test
  (is
   (language-match?
    (:language-range (first (reap/accept-language "en")))
    (:langtag (first (reap/content-language "en")))))

  (is
   (language-match?
    (:language-range (first (reap/accept-language "de-de")))
    (:langtag (first (reap/content-language "de-DE-1996")))))

  (is
   (not
    (language-match?
     (:language-range (first (reap/accept-language "de-de")))
     (:langtag (first (reap/content-language "de-Latn-DE"))))))

  (is
   (not
    (language-match?
     (:language-range (first (reap/accept-language "en-gb")))
     (:langtag (first (reap/content-language "en"))))))

  (is
   (language-match?
    (:language-range (first (reap/accept-language "*")))
    (:langtag (first (reap/content-language "de"))))))


(reap/content-language "de,en")

;; RFC 7231 Section 5.3.5:
;; For example,

;;      Accept-Language: da, en-gb;q=0.8, en;q=0.7

;;    would mean: "I prefer Danish, but will accept British English and
;;    other types of English".


#_(deftest acceptable-language-rating-test
  (are [language expected]
      (= (select-keys

          [:qvalue :precedence])
         expected)
    "text/html;charset=utf-8" {:precedence 3 :qvalue 0.1}
    "text/html;level=2;charset=utf-8" {:precedence 4 :qvalue 0.4}))

#_(acceptable-language-rating
 (reap/accept-language "en")
 (reap/content-language "en"))

(deftest select-most-acceptable-representation-test

  (are [accept-header expected-content]
      (= expected-content
         (:id
          (select-most-acceptable-representation
           (-> (request :get "/hello")
               (update
                :headers conj
                ["accept" accept-header]))

           [{:id :html
             :apex.http/content "<h1>Hello World!</h1>"
             :apex.http/content-type "text/html;charset=utf-8"}

            {:id :html-level-2
             :apex.http/content "<h1>Hello World!</h1>"
             :apex.http/content-type "text/html;level=2;charset=utf-8"}

            {:id :plain-text
             :apex.http/content "Hello World!"
             :apex.http/content-type "text/plain;charset=utf-8"}])))

    "text/html" :html
    "TEXT/HTML" :html

    "text/html;q=0.8,text/plain;q=0.7" :html
    "text/plain" :plain-text
    "text/html;q=0.8,text/plain" :plain-text

    "TEXT/HTML;level=2;text/html;q=0.8" :html-level-2))

;; TODO: Test quality-of-source

(deftest language-test
  (let [variants
        [{:id :en
          :apex.http/content "Hello!"
          :apex.http/content-language "en"}

         {:id :en-us
          :apex.http/content-language "en-US"
          ;; https://en.wikipedia.org/wiki/Howdy
          ;; Not everyone in the US uses 'Howdy!' but this is just a test...
          :apex.http/content "Howdy!"}

         {:id :ar-eg
          :apex.http/content-language "ar-eg,en"
          :apex.http/content "ألسّلام عليكم"}]]

    (are [accept-header expected]
        (=
         expected
         (map
          (juxt :id :apex.http.content-negotiation/language-qvalue)
          (sequence
           (assign-language-quality
            (reap/accept-language accept-header))
           variants)))

        "en" [[:en 1.0][:en-us 1.0][:ar-eg 0.0]]
        "en-us" [[:en 0.0][:en-us 1.0][:ar-eg 0.0]]
        "ar-eg" [[:en 0.0][:en-us 0.0][:ar-eg 1.0]]
        "en-us,en;q=0.8,ar-eg;q=0.2" [[:en 0.8][:en-us 1.0][:ar-eg 0.2]]
        "*" [[:en 1.0][:en-us 1.0][:ar-eg 1.0]]
        "en-us,*;q=0.1" [[:en 0.1][:en-us 1.0][:ar-eg 0.1]])

    (are [accept-header expected]
        (= expected
           (:apex.http/content
            (select-most-acceptable-representation
             (-> (request :get "/hello")
                 (update
                  :headers conj
                  ["accept-language" accept-header]))
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
        "en-us;q=0.8,*" "Hello!")

    ;; If no Accept-Language header, just pick the first variant.
    (is (= "Hello!"
           (:apex.http/content
            (select-most-acceptable-representation
             (-> (request :get "/hello"))
             variants))
           ))

    ;; The language quality factor of a variant, if present, is used in
    ;; preference to an Accept-Language header.
    (is
     (=
      "Bonjour!"
      (:apex.http/content
       (select-most-acceptable-representation
        (-> (request :get "/hello")
            (update
             :headers conj
             ["accept-language" "en"]))
        (conj
         variants
         {:id :fr-fr
          :apex.http/content "Bonjour!"
          :apex.http/content-language "fr-FR"
          :apex.http/language-quality-factor 2})))))))

;; TODO: Add language quality factor in variants: e.g. apex.http/language-quality-factor 0.8 - easy to test for
