(var pr_str nil)

(defn pr_list [mal-data]
  (def parens
    {:list ["(" ")"]
     :vector ["[" "]"]
     :hash-map ["{" "}"]})
  (def ss (map pr_str (mal-data :value)))
  (def p (parens (mal-data :kind)))
  (string (p 0) (string/join ss " ") (p 1)))

(set pr_str
     (fn [mal-data]
       (match mal-data
         {:type :atom} (string (mal-data :value))
         {:type :list} (pr_list mal-data)
         {:type :comment} ""
         {:type :nil} "nil"
         (error mal-data))
       ))

