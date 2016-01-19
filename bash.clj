(defmulti emit-bash
      (fn [form]
          (class form)))

(defn emit-def [form]
  (let [[kw variable value] form]
    (str variable "=" (emit-bash value))))

(defmethod emit-bash
      clojure.lang.PersistentList
      [form]
      (case (name (first form))
            "def" (emit-def form)
            "println" (str "echo " (emit-bash (second form)))
            "if" (str "if [" (emit-bash (second form)) "]; then\n  " (emit-bash (nth form 2)) "; \nfi")
            "="  (str " " (emit-bash (second form)) " == " (emit-bash (nth form 2)) " ")
              nil))

(defmethod emit-bash
      clojure.lang.Symbol
      [form]
      (str "\"" form "\""))

(defmethod emit-bash
      java.lang.String
      [form]
      (str "\"" form "\""))

(defmethod emit-bash
      java.lang.Long
      [form]
      (str form))

(defmethod emit-bash
      java.lang.Integer
      [form]
      (str form))

(defmethod emit-bash
      java.lang.Double
      [form]
      (str form))

(println
  (emit-bash '(def "XCODE_PROVISIONING" 2)))
