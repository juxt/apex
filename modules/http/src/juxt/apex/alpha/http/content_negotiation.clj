;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.alpha.http.content-negotiation
  (:require
   [juxt.reap.alpha.api :as reap]))

(defn- content-type-match?
  "Return truthy if the given accept-field (reap format) accepts the given
  content-type (reap format). The return value is a the precedence value (if
  matched), nil otherwise."
  [parsed-accept-field parsed-content-type]
  (cond
    (and
     (= (:type parsed-accept-field) (:type parsed-content-type))
     (= (:subtype parsed-accept-field) (:subtype parsed-content-type))
     ;; Try to match on all the parameters asked for in the accept,
     ;; but discard all others in the content type.
     (pos? (count (:parameters parsed-accept-field)))
     (= (:parameters parsed-accept-field)
        (select-keys
         (:parameters parsed-content-type)
         (keys (:parameters parsed-accept-field)))))
    ;; The precedence could be 3, plus the number of parameters in the
    ;; accept. For now, we don't include the count of the parameters
    ;; in the determination of precedence.
    4

    (and
     (= (:type parsed-accept-field) (:type parsed-content-type))
     (= (:subtype parsed-accept-field) (:subtype parsed-content-type))
     (zero? (count (:parameters parsed-accept-field))))
    3

    (and
     (= (:type parsed-accept-field) (:type parsed-content-type))
     (= "*" (:subtype parsed-accept-field)))
    2

    (and
     (= "*" (:type parsed-accept-field))
     (= "*" (:subtype parsed-accept-field)))
    1))

(defn- select-better-content-type-match
  "Designed to be used as a reducing function, if the parsed-accept-field is a
  higher precedence (or same precedence with higher qvalue), return an updated
  best-match map."

  [best-match parsed-accept-field]

  (let [precedence (content-type-match? parsed-accept-field (:content-type best-match))
        qvalue (get parsed-accept-field :qvalue 1.0)]

    (cond-> best-match
      (and
       precedence
       (or
        (> precedence (get best-match :precedence 0))
        (and (= precedence (get best-match :precedence 0))
             (> qvalue (get best-match :qvalue 0.0)))))
      (conj
       [:qvalue qvalue]
       [:precedence precedence]
       [:apex.debug/parsed-accept-field parsed-accept-field]))))

(defn acceptable-content-type-rating
  "Determine the given content-type's rating (precedence, qvalue) with respect to
  what is acceptable. The parsed-accept-fields parameter is a data structure
  returned from parsing the Accept header with reap. The variant is a map
  corresponding to the resource of the variant.

  This function determines the qvalue according to rules of precedence in RFC
  7231 Section 5.3.2 which are independent of the actual content negotation
  used.

  The qvalue is set to 0 if 'not acceptable' (See RFC 7231 Section 5.3.1). This
  still gives the content negotiation algorithm the chance of returning a
  variant, if there are no more preferable variants and if returning one is
  preferable to returning a 406 status code."

  [parsed-accept-fields parsed-content-type]

  (reduce
   select-better-content-type-match
   {:qvalue 0.0
    :content-type parsed-content-type}
   parsed-accept-fields))

(defn debug-acceptable-content-type-rating
  "Same as acceptable-content-type-rating but with reductions, for debugging."
  [parsed-accept-fields parsed-content-type]
  (reductions
   select-better-content-type-match
   {:qvalue 0.0
    :content-type parsed-content-type}
   parsed-accept-fields))

(defn language-match?
  "Basic filtering as per RFC 4647 Section 3.3.1."
  [^String language-range ^String language-tag]

  (assert language-range)
  (assert (string? language-range))
  (assert language-tag)
  (assert (string? language-tag))

  (or
   (and
    ;; "A language range matches a particular language tag if, in a
    ;; case-insensitive comparison, it exactly equals the tag, …"
    (.regionMatches language-range true 0 language-tag 0 (. language-range length))

    ;; "or if it exactly equals a prefix of the tag such that the first character
    ;; following the prefix is '-'. "
    (if (< (count language-range) (count language-tag))
      (= \- (.charAt language-tag (count language-range)))
      true))

   ;; "The special range '*' in a language priority list matches any tag."
   (.equals language-range "*")))

(defn- select-better-language-match
  [best-match parsed-accept-language-field]
  (let [qvalue (get parsed-accept-language-field :qvalue 1.0)]
    (cond-> best-match
      (and (language-match?
            (:language-range parsed-accept-language-field)
            (get-in best-match [:language-tag :langtag]))
           (> qvalue (get best-match :qvalue 0.0)))
      (conj
       [:qvalue qvalue]
       [:apex.debug/parsed-accept-language-field parsed-accept-language-field]))))

(defn acceptable-language-rating
  "Determine the given language's rating (precedence, qvalue) with respect to what
  is acceptable. The parsed-accept-language-fields parameter is a data structure
  returned from parsing the Accept-Language header with reap. The variant is a
  map corresponding to the resource of the variant.

  This function determines the qvalue according to rules of precedence in RFC
  7231 Section 5.3.2 which are independent of the actual content negotation
  used.

  The qvalue is set to 0 if 'not acceptable' (See RFC 7231 Section 5.3.1). This
  still gives the content negotiation algorithm the chance of returning a
  variant, if there are no more preferable variants and if returning one is
  preferable to returning a 406 status code."

  [parsed-accept-language-fields parsed-language-tag]

  (reduce
   select-better-language-match
   {:qvalue 0.0
    :language-tag parsed-language-tag}
   parsed-accept-language-fields))

;; Apache httpd Negotiation Algorithm -- http://httpd.apache.org/docs/current/en/content-negotiation.html#algorithm

;; Dimensions:
;;
;; media-type
;; language
;; encoding
;; charset

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

(defn assign-content-type-quality [parsed-accept-fields]
  (keep
   (fn [variant]
     (let [qvalue
           (when-let [content-type (:apex.http/content-type variant)]
             (:qvalue
              (acceptable-content-type-rating
               parsed-accept-fields
               (reap/content-type content-type))))]
       (cond-> variant
         qvalue (conj [:apex.http.content-negotiation/content-type-qvalue qvalue]))))))

(defn assign-language-quality [parsed-accept-language-fields]
  (keep
   (fn [variant]
     (let [qvalue
           (when-let [content-language (:apex.http/content-language variant)]
             (:qvalue
              (acceptable-language-rating
               parsed-accept-language-fields
               ;; Content languages can be lists of language tags for the
               ;; 'intended audience'. But for the purposes of language
               ;; negotiation, we pick the FIRST content-language in the
               ;; list. The matching of multiple languages with a language tag
               ;; is not defined by any RFC (as far as I can tell).
               (first
                (reap/content-language content-language)))))]
       (cond-> variant
         qvalue (conj [:apex.http.content-negotiation/language-qvalue qvalue]))))))

(defn rate-variants [request variants]
  (sequence

   (comp

    (assign-content-type-quality
     (reap/accept
      (get-in
       request
       [:headers "accept"]
       ;; "A request without any Accept header field implies that the user
       ;; agent will accept any media type in response". -- test for this
       ;; -- RFC 7231 Section 5.3.2
       "*/*")))

    (assign-language-quality
     (reap/accept-language
      (get-in
       request
       [:headers "accept-language"]
       ;; "A request without any Accept-Language header field implies that the
       ;; user agent will accept any language in response."
       ;; -- RFC 7231 Section 5.3.5
       "*"))))

   ;; TODO: Repeat for other dimensions. Short circuit, so if 0 variants left,
   ;; don't keep parsing headers! But with one left, keep parsing because maybe
   ;; that will be eliminated too! Keep in mind that 406 is discouraged for
   ;; unacceptable languages: "or honor the header field by sending a 406 (Not
   ;; Acceptable) response.  However, the latter is not encouraged, as doing so
   ;; can prevent users from accessing content that they might be able to use
   ;; (with translation software, for example). -- RFC 7231 Section 5.3.5"

   variants))

(defn select-max-by
  "Return the items in the collection that share the maximum numeric value
  returned by calling the given 1-arity keyfn function with the item as the
  argument. This is commonly used when negotiating representations based on
  process-of-elimination techniques."
  [keyfn coll]
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

(defn select-most-acceptable-representation

  "Implementation of the Apache httpd content-negotiation algorithm detailed at
  https://httpd.apache.org/docs/current/en/content-negotiation.html#algorithm"

  [request variants]

  (let [representations (rate-variants request variants)]

    (reduce
     (fn [variants step]
       ;; Short-circuit the algorithm when 0 or 1 representation remains.
       (if (< (count variants) 2)
         (reduced (first variants))
         (step variants)))

     representations

     ;; Algorithm steps
     [(fn [variants]
        ;; "Multiply the quality factor from the Accept header with the
        ;; quality-of-source factor for this variants media type, and select
        ;; the variants with the highest value."
        (select-max-by
         (fn [variant]
           (* (get variant :apex.http.content-negotiation/content-type-qvalue 1.0)
              (get variant :apex.http/quality-of-source 1.0)))
         variants))

      ;; Select the variants with the highest language quality factor.
      (fn [variants]
        (select-max-by #(get % :apex.http/language-quality-factor 1.0) variants))

      ;; Select the variants with the best language match
      (fn [variants]
        (select-max-by :apex.http.content-negotiation/language-qvalue variants))

      ;; TODO: Select the variants with the highest 'level' media parameter (used to give the version of text/html media types).
      ;; TODO: Select variants with the best charset media parameters, as given on the Accept-Charset header line.
      ;; TODO: Select those variants which have associated charset media parameters that are not ISO-8859-1.
      ;; TODO: Select the variants with the best encoding.
      ;; TODO: Select the variants with the smallest content length.

      ;; Select the first variant of those remaining
      first])))

;; TODO: Produce an 'explain' for each content negotiation that can be
;; logged/response on a 406 (and to support debugging)
