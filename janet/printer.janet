(var pr_str nil)

(defn- pr_list [mal-data]
  (def parens
    {:list ["(" ")"]
     :vector ["[" "]"]
     :hash-map ["{" "}"]})
  (def ss (map pr_str (mal-data :value)))
  (def p (parens (mal-data :kind)))
  (string (p 0) (string/join ss " ") (p 1)))

(defn- pr_atom [mal-data]
  (def val (mal-data :value))
  (match mal-data
         {:kind :maybe-string} (if (string/has-suffix? "\"" val)
                                 (string val)
                                 (error {:type :error :kind :EOF}))
         (string val)))

(set pr_str
     (fn [mal-data]
       (match mal-data
              {:type :atom} (pr_atom mal-data)
              {:type :list} (pr_list mal-data)
              {:type :comment} ""
              {:type :nil} "nil"
              (error mal-data))
       ))

