;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.http.content-negotiation-test
  (:require
   [juxt.reap.alpha.api :as reap]
   [ring.mock.request :refer [request]]
   [clojure.test :refer [deftest is are]]))

;; See http://httpd.apache.org/docs/current/en/content-negotiation.html#algorithm

(defprotocol Variant
  ;; TODO: Check RFC for correct naming (should this by media-type?)
  (content-type [_] "Return content type (with charset parameter).")
  (quality-of-source [_]))

(extend-protocol Variant
  clojure.lang.APersistentMap
  (content-type [m] (:apex.http/content-type m))
  (quality-of-source [m] (:apex.http/quality-of-source m))
  String
  (content-type [s] s)
  (quality-of-source [s] 1))

(defn acceptable-media-type
  [accepts variant]
  (let [content-type (reap/content-type (content-type variant))]
    (reduce
     (fn [acc accept]
       (if-let
           [precedence
            (cond
              (and
               (= (:type accept) (:type content-type))
               (= (:subtype accept) (:subtype content-type)))
              (if (pos? (count (:parameters accept)))
                (when (= (:parameters accept) (:parameters content-type)) 4)
                3)

              (and
               (= (:type accept) (:type content-type))
               (= "*" (:subtype accept)))
              2

              (and
               (= "*" (:type accept))
               (= "*" (:subtype accept)))
              1)]

           (let [qvalue (get accept :qvalue 1.0)
                 quality-factor (*
                                 qvalue
                                 (or (quality-of-source variant) 1))]
             (if (or
                  (> precedence (get acc :precedence 0))
                  (and (= precedence (get acc :precedence 0))
                       (> quality-factor (get acc :quality-factor 0.0))))

               {:accept accept
                :qvalue qvalue
                :quality-factor quality-factor
                :precedence precedence}

               acc))
           acc))
     nil
     accepts)))

(comment
  (acceptable-media-type
   (reap/accept "text/*;q=0.3, text/html;q=0.7, text/html;level=1, text/html;level=2;q=0.4")
   "image/png"))

(comment
  (acceptable-media-type
   (reap/accept "text/*;q=0.3, text/html;q=0.7, text/html;level=1, text/html;level=2;q=0.4, */*;q=0.5")
   "text/html;level=3"))

#_(let [accepts (reap/accept "text/*;q=0.3, text/html;q=0.7, text/html;level=1, text/html;level=2;q=0.4, */*;q=0.5")]
  (acceptable-media-type
   accepts
   "text/plain"))

(deftest acceptable-media-type-test
  (let [accepts (reap/accept "text/*;q=0.3, text/html;q=0.7, text/html;level=1, text/html;level=2;q=0.4, */*;q=0.5")]
    (is (= 1.0 (:qvalue
                (acceptable-media-type
                 accepts
                 "text/html;level=1"))))
    (is (= 0.7 (:qvalue
                (acceptable-media-type
                 accepts
                 "text/html"))))
    (is (= 0.3 (:qvalue
                (acceptable-media-type
                 accepts
                 "text/plain"))))
    (is (= 0.5 (:qvalue
                (acceptable-media-type
                 accepts
                 "image/jpeg"))))
    (is (= 0.4 (:qvalue
                (acceptable-media-type
                 accepts
                 "text/html;level=2"))))
    (is (= 0.7 (:qvalue
                (acceptable-media-type
                 accepts
                 "text/html;level=3"))))))

;; TODO: Reap should return lower-case to ensure comparisons work
;; case-insensitively. String's equalsIgnoreCase is not sufficient because
;; parameter maps need to be compared for quality.

;; Dimensions:
;;
;; media-type
;; language
;; encoding
;; charset (avoid)

(defn select-max-by [keyfn coll]
  (:items
   (reduce
    (fn [acc item]
      (let [val (or (keyfn item) 0)
            current-max (:val acc)]
        (cond
          (> val current-max)
          (assoc acc
                 :items [item]
                 :val val)

          (= val current-max)
          (update acc :items conj item)

          :else acc)))
    {:items [] :val 0} coll)))

(defn select-best-representation [request variants]

  (let [assign-media-type-quality
        (fn [accepts]
          (fn [variant]
            (let [quality (acceptable-media-type accepts variant)]
              (cond-> variant
                quality (conj [:apex.http.content-negotiation/media-type-quality quality])))))]

    (reduce
     (fn [variants step]
       (if (< (count variants) 2)
         (reduced (first variants))
         (step variants)))

     ;; Accumulator
     (->> variants
          (keep
           ;; Multiply the quality factor from the Accept header with the
           ;; quality-of-source factor for this variants media type, and select
           ;; the variants with the highest value.
           (assign-media-type-quality
            (reap/accept
             (get-in
              request
              [:headers "accept"]
              ;; "A request without any Accept header field implies that the user
              ;; agent will accept any media type in response". -- test for this
              "*/*"))))
          ;; TODO: repeat for other dimensions. Short circuit, so if 0 variants left,
          ;; don't keep parsing headers! But with one left, keep parsing because
          ;; maybe that will be eliminated too!
          )

     ;; Steps
     [(fn [variants]
        (select-max-by
         (comp :quality-factor :apex.http.content-negotiation/media-type-quality)
         variants))
      ;; TODO: Select the variants with the highest language quality factor.
      ;; TODO: Select the variants with the best language match
      ;; TODO: Select the variants with the highest 'level' media parameter (used to give the version of text/html media types).
      ;; TODO: Select variants with the best charset media parameters, as given on the Accept-Charset header line.
      ;; TODO: Select those variants which have associated charset media parameters that are not ISO-8859-1.
      ;; TODO: Select the variants with the best encoding.
      ;; TODO: Select the variants with the smallest content length.
      ;; TODO: Select the first variant of those remaining
      first])))

(select-best-representation
 (-> (request :get "/hello")
     (update
      :headers conj
      ["accept" "text/html;q=0.8,text/plain;q=0.7"]
      ["accept-language" "en"]))

 [{:apex.http/content "<h1>Hello World!</h1>"
   :apex.http/content-type "text/html;charset=utf-8"
   }
  {:apex.http/content "Hello World!"
   :apex.http/content-type "text/plain;charset=utf-8"
   }
  {:apex.http/content "{\"message\": \"Hello World!\"}"
   :apex.http/content-type "application/json"
   }])



;; httpd Negotiation Algorithm

;; httpd can use the following algorithm to select the 'best' variant (if any) to return to the browser. This algorithm is not further configurable. It operates as follows:

;;     First, for each dimension of the negotiation, check the appropriate Accept* header field and assign a quality to each variant. If the Accept* header for any dimension implies that this variant is not acceptable, eliminate it. If no variants remain, go to step 4.
;;     Select the 'best' variant by a process of elimination. Each of the following tests is applied in order. Any variants not selected at each test are eliminated. After each test, if only one variant remains, select it as the best match and proceed to step 3. If more than one variant remains, move on to the next test.
;;         Multiply the quality factor from the Accept header with the quality-of-source factor for this variants media type, and select the variants with the highest value.
;;         Select the variants with the highest language quality factor.
;;         Select the variants with the best language match, using either the order of languages in the Accept-Language header (if present), or else the order of languages in the LanguagePriority directive (if present).
;;         Select the variants with the highest 'level' media parameter (used to give the version of text/html media types).
;;         Select variants with the best charset media parameters, as given on the Accept-Charset header line. Charset ISO-8859-1 is acceptable unless explicitly excluded. Variants with a text/* media type but not explicitly associated with a particular charset are assumed to be in ISO-8859-1.
;;         Select those variants which have associated charset media parameters that are not ISO-8859-1. If there are no such variants, select all variants instead.
;;         Select the variants with the best encoding. If there are variants with an encoding that is acceptable to the user-agent, select only these variants. Otherwise if there is a mix of encoded and non-encoded variants, select only the unencoded variants. If either all variants are encoded or all variants are not encoded, select all variants.
;;         Select the variants with the smallest content length.
;;         Select the first variant of those remaining. This will be either the first listed in the type-map file, or when variants are read from the directory, the one whose file name comes first when sorted using ASCII code order.
;;     The algorithm has now selected one 'best' variant, so return it as the response. The HTTP response header Vary is set to indicate the dimensions of negotiation (browsers and caches can use this information when caching the resource). End.
;;     To get here means no variant was selected (because none are acceptable to the browser). Return a 406 status (meaning "No acceptable representation") with a response body consisting of an HTML document listing the available variants. Also set the HTTP Vary header to indicate the dimensions of variance.
