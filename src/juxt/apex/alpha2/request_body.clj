(ns juxt.apex.alpha2.request-body
  (:require
   [juxt.jinx-alpha :as jinx]
   [juxt.apex.alpha2.codec :as codec]
   [muuntaja.core :as m]))

(def wrap-process-request-body
  {:name "Process request body, which may involve decoding the stream according to charset and doing schema validation/coercion"
   :compile
   (fn [route-data router-opts]

     ;; Only PUT and POST allow message bodies in requests. Custom
     ;; methods are out of scope of OpenAPI and this library and may
     ;; have to provide their own body handling middleware.
     (when (#{:put :post} (:apex/method route-data))

       ;; NOTE: Should it be possible to 'coerce' another
       ;; content-type, say application/edn, to application/json ? If
       ;; so, I think this should be done, optionally, in a prior
       ;; middleware.

       ;; TODO: Compile all schemas if possible
       ;; where's the schema?

       (let [schemas-by-type
             (when-let [content (get-in route-data [:apex/operation "requestBody" "content"])]
               (reduce-kv
                (fn [acc content-type {:strs [schema]}]
                  (assoc acc content-type
                         ;; Nil schema is OK, we'll just allow and pass through
                         (when schema (jinx/schema
                                       schema
                                       ))))
                {}
                content)
               )]

         (fn [h]
           (fn
             ([req]
              (assert (string? (:body req)) "TODO: handle input-streams, channels, etc.") ; read up on Jetty

              (if-let [content-type
                       ;; TODO: Of course, we should parse this header
                       ;; properly according to the RFCs. Also, we
                       ;; should use a delay to ensure that this
                       ;; header value is not re-parsed multiple times
                       ;; in a request.
                       (get-in req [:headers "content-type"])]

                ;; Use Muuntaja to decode the body according to
                ;; the content-type
                ;;
                ;; TODO: Be able to specify a custom Muuntaja instance
                ;; here.
                ;;
                ;; TODO: We must also cope with the situation that the
                ;; content-type is not supported by this resource.

                (let [payload (m/decode codec/default-muuntaja content-type (:body req))]


                  (if-let [schema (get schemas-by-type content-type)]
                    ;; Validate according to schema
                    (let [validation
                          (jinx/validate
                           payload schema
                           ;; TODO: Since the $ref is in the schema, not
                           ;; the instance, it feels as if we should be
                           ;; able to provide the base-document in the
                           ;; compiled schema, ahead-of-time.
                           {:base-document (:apex/openapi route-data)})]
                      (if (:valid? validation)
                        ;; The validation instance will contain the
                        ;; results of default values (and any coercions).
                        (h (assoc req :apex/parsed-body (:instance validation)))

                        {:status 400
                         ;; TODO: Need an explain which will use the validation
                         :body "Invalid payload"
                         :apex/payload payload
                         :jinx/validation validation}))

                    ;; If there isn't a schema, just pass through? What
                    ;; if we've already streamed and maybe parsed the body?
                    (h req)))

                ;; No matching content-type declared in content, therefore this is illegal
                {:status 415
                 ;; TODO: We'll have dev support for this which will
                 ;; give an explain
                 :body "Unsupported Media Type"}))

             ([req respond raise]
              (println "async: coerce-body")
              (h req respond raise)))))))})
