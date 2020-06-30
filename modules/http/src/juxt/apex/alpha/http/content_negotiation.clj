;; Copyright © 2020, JUXT LTD.

(ns juxt.apex.alpha.http.content-negotiation
  (:require
   [juxt.reap.alpha.api :as reap]
   [clojure.string :as str]))

(defn match-parameters?
  "Return true if all parameters in the accept parameters, are matched by values
  in the content map. Keys are case insensitive, but always lower-case in the
  content-map."
  [accept-map content-map]
  (loop [[[k v] & accept-entries] accept-map]
    (if k
      (when (= v (get content-map (str/lower-case k)))
        (recur accept-entries))
      true)))

(defn- content-type-match?
  "Return truthy if the given accept-field (reap format) accepts the given
  content-type (reap format). The return value is a the precedence value (if
  matched), nil otherwise."
  [parsed-accept-field parsed-content-type]
  (cond
    (and
     (.equalsIgnoreCase (:juxt.http/type parsed-accept-field) (:juxt.http/type parsed-content-type))
     (.equalsIgnoreCase (:juxt.http/subtype parsed-accept-field) (:juxt.http/subtype parsed-content-type))
     ;; Try to match on all the parameters asked for in the accept,
     ;; but discard all others in the content type.
     (pos? (count (:juxt.http/parameters parsed-accept-field)))
     ;; TODO:
     (match-parameters?
      (:juxt.http/parameters parsed-accept-field)
      (:juxt.http/parameter-map parsed-content-type)))

    ;; The precedence could be 3, plus the number of parameters in the
    ;; accept. For now, we don't include the count of the parameters
    ;; in the determination of precedence.
    4

    (and
     (.equalsIgnoreCase (:juxt.http/type parsed-accept-field) (:juxt.http/type parsed-content-type))
     (.equalsIgnoreCase (:juxt.http/subtype parsed-accept-field) (:juxt.http/subtype parsed-content-type))
     (zero? (count (:juxt.http/parameters parsed-accept-field))))
    3

    (and
     (.equalsIgnoreCase (:juxt.http/type parsed-accept-field) (:juxt.http/type parsed-content-type))
     (= "*" (:juxt.http/subtype parsed-accept-field)))
    2

    (and
     (= "*" (:juxt.http/type parsed-accept-field))
     (= "*" (:juxt.http/subtype parsed-accept-field)))
    1))

(defn- select-better-content-type-match
  "Designed to be used as a reducing function, if the parsed-accept-field is a
  higher precedence (or same precedence with higher qvalue), return an updated
  best-match map."

  [best-match parsed-accept-field]

  (let [precedence (content-type-match? parsed-accept-field (:content-type best-match))
        qvalue (get parsed-accept-field :juxt.http/qvalue 1.0)]

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

;; TODO: Support nil arg
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

(defn acceptable-charset-rating
  [parsed-accept-charset-fields parsed-content-type]
  (when-let [charset (get-in parsed-content-type [:juxt.http/parameter-map "charset"])]
    (reduce
     (fn [best-match field]
       (cond
         (= charset (:juxt.http/charset field))
         (cond-> best-match
           (< (get best-match :precedence) 2)
           (conj [:qvalue (get field :juxt.http/qvalue 1.0)]
                 [:precedence 2]
                 [:apex.debug/parsed-accept-charset-field field]))
         (= "*" (:juxt.http/charset field))
         (cond-> best-match
           (= (get best-match :precedence) 0)
           (conj [:qvalue (get field :juxt.http/qvalue 1.0)]
                 [:precedence 1]
                 [:apex.debug/parsed-accept-charset-field field]))
         :else best-match))
     {:qvalue 0.0
      :precedence 0}
     parsed-accept-charset-fields)))

(defn basic-language-match?
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
  (let [qvalue (get parsed-accept-language-field :juxt.http/qvalue 1.0)]
    (cond-> best-match
      (and
       (> qvalue (get best-match :qvalue 0.0))
       (basic-language-match?
        (:juxt.http/language-range parsed-accept-language-field)
        (get-in best-match [:language-tag :juxt.http/langtag])))
      (conj
       [:qvalue qvalue]
       [:apex.debug/parsed-accept-language-field parsed-accept-language-field]))))

;; TODO: Support nil arg
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

  ;; TODO: Improve this function by allowing multiple language tags and using
  ;; multiplication.

  [parsed-accept-language-fields parsed-language-tag]

  (reduce
   select-better-language-match
   {:qvalue 0.0
    :language-tag parsed-language-tag}
   parsed-accept-language-fields))

(defn select-best-encoding-match [accept-encoding-fields entry]
  (reduce
   (fn [best-match {accept-coding :juxt.http/codings :as field}]

     (cond
       (= accept-coding (get entry :juxt.http/content-coding "identity"))
       (cond-> best-match
         (< (get best-match :precedence) 2)
         (conj [:qvalue (get field :juxt.http/qvalue 1.0)]
               [:precedence 2]
               [:apex.debug/parsed-accept-encoding-field field]))

       (= accept-coding "*")
       (cond-> best-match
         (= (get best-match :precedence) 0)
         (conj [:qvalue (get field :juxt.http/qvalue 1.0)]
               [:precedence 1]
               [:apex.debug/parsed-accept-encoding-field field]))

       :else best-match))

   {:precedence 0
    :qvalue (if
                ;; "If the representation has no content-coding, then it is
                ;; acceptable by default unless specifically excluded by the
                ;; Accept-Encoding field stating either 'identity;q=0' or
                ;; '*;q=0' without a more specific entry for 'identity'."
                ;;
                ;; -- RFC 7231 Section 5.3.4
                (= (get entry :juxt.http/content-coding "identity") "identity")
              1.0
              0.0)}

   accept-encoding-fields))


(defn acceptable-encoding-qvalue
  "Determine the qvalue for the given parsed content-encoding according to the
  given parsed Accept-Encoding header fields.

  The content-encoding can be nil.

  > If the representation has no content-coding, then it is acceptable by
  default unless specifically excluded by the Accept-Encoding field stating
  either 'identity;q=0' or '*;q=0' without a more specific entry for 'identity'.
  -- RFC 7231 Section 5.3.4

  "
  [parsed-accept-encoding-fields parsed-content-encoding]

  (double
   (reduce
    ;; For content-encodings with multiple codings, it feels sensible to
    ;; multiply the qvalues together. Any unsupported coding will yield a total
    ;; qvalue 0.0, while if all qvalues are 1.0, the total will be 1.0.
    *
    (for [entry parsed-content-encoding]
      (:qvalue
       (select-best-encoding-match parsed-accept-encoding-fields entry))))))

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

;; TODO: Support nil arg
(defn assign-content-type-quality [accept-fields-or-header]
  (let [parsed-accept-fields
        (reap/accept-when-string accept-fields-or-header)]
    (keep
     (fn [variant]
       (let [qvalue
             (when-let [content-type (:juxt.http/content-type variant)]
               (:qvalue
                (acceptable-content-type-rating
                 parsed-accept-fields
                 (reap/content-type-when-string content-type))))]
         (cond-> variant
           qvalue (conj [:juxt.http.content-negotiation/content-type-qvalue qvalue])))))))

;; TODO: Support nil arg
(defn assign-language-quality [accept-language-fields-or-header]
  (let [parsed-accept-language-fields
        (reap/accept-language-when-string accept-language-fields-or-header)]
    (keep
     (fn [variant]
       (let [qvalue
             (when-let [content-language (:juxt.http/content-language variant)]
               (:qvalue
                (acceptable-language-rating
                 parsed-accept-language-fields
                 ;; Content languages can be lists of language tags for the
                 ;; 'intended audience'. But for the purposes of language
                 ;; negotiation, we pick the FIRST content-language in the
                 ;; list. The matching of multiple languages with a language tag
                 ;; is not defined by any RFC (as far as I can tell).
                 ;;
                 ;; TODO: We should now use the accept-encoding method of
                 ;; arriving at the combined quality factor via multiplication.
                 (first
                  (reap/content-language-when-string content-language)))))]
         (cond-> variant
           qvalue (conj [:juxt.http.content-negotiation/language-qvalue qvalue])))))))

(defn assign-encoding-quality
  "Returns a transducer that will apply a rating on each of a collection of
  variants, according to the given parsed Accept-Encoding fields. This argument
  can be nil, which is interpretted to mean that no Accept-Encoding header is
  present."
  [accept-encoding-fields-or-header]
  (let [accept-encoding-fields
        (reap/accept-encoding-when-string accept-encoding-fields-or-header)]
    (keep
     (fn [variant]
       (let [qvalue
             (if accept-encoding-fields
               (acceptable-encoding-qvalue
                accept-encoding-fields
                (reap/content-encoding-when-string
                 (get variant :juxt.http/content-encoding "identity")))

               ;; "If no Accept-Encoding field is in the request, any
               ;; content-coding is considered acceptable by the user agent."
               ;; -- RFC 7231 Section 5.3.4
               1.0)]
         (cond-> variant
           qvalue (conj [:juxt.http.content-negotiation/encoding-qvalue qvalue])))))))

(defn rate-variants [request variants]
  (sequence

   (comp

    (assign-content-type-quality
     (get-in
      request
      [:headers "accept"]
      ;; "A request without any Accept header field implies that the user
      ;; agent will accept any media type in response". -- test for this
      ;; -- RFC 7231 Section 5.3.2
      "*/*"))

    (assign-language-quality
     (reap/accept-language
      (get-in
       request
       [:headers "accept-language"]
       ;; "A request without any Accept-Language header field implies that the
       ;; user agent will accept any language in response."
       ;; -- RFC 7231 Section 5.3.5
       "*")))

    (assign-encoding-quality
     (reap/accept-encoding
      (get-in
       request
       [:headers "accept-encoding"]))))

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
           (* (get variant :juxt.http.content-negotiation/content-type-qvalue 1.0)
              (get variant :juxt.http/quality-of-source 1.0)))
         variants))

      ;; Select the variants with the highest language quality factor.
      (fn [variants]
        (select-max-by #(get % :juxt.http/language-quality-factor 1.0) variants))

      ;; Select the variants with the best language match
      (fn [variants]
        (select-max-by :juxt.http.content-negotiation/language-qvalue variants))

      ;; TODO: Select the variants with the highest 'level' media parameter (used to give the version of text/html media types).

      ;; TODO: Select variants with the best charset media parameters, as given on the Accept-Charset header line.

      ;; TODO: Select those variants which have associated charset media parameters that are not ISO-8859-1.

      ;; Select the variants with the best encoding.
      (fn [variants]
        (select-max-by :juxt.http.content-negotiation/encoding-qvalue variants))

      ;; TODO: Select the variants with the smallest content length.

      ;; Select the first variant of those remaining
      first])))

;; TODO: Produce an 'explain' for each content negotiation that can be
;; logged/response on a 406 (and to support debugging). Perhaps as a 406 body
;; but also by using an Expect (which is a 'must understand' semantic) or Prefer
;; header (which isn't)? See RFC 7240.
