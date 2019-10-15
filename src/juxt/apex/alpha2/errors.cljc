(ns juxt.apex.alpha2.errors)

(defn error? [m]
  (when (associative? m)
    (contains? m :apex/error)))

(defmacro if-error [sym on-error & body]
  (assert (.endsWith (str sym) "+"))
  (let [s (symbol (subs (str sym) 0 (dec (count (str sym)))))
        error (symbol "error")]
    `(if (error? ~sym)
       (let [~error (:apex/error ~sym)]
         ~on-error)
       (let [~s ~sym]
         ~@body))))

#_(let [e! {::message2 "foo"}]
    (if-error e!
      (println "error")
      (println "e is" e))
    )
