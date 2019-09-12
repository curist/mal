(var pr_str nil)

(defn pr_list [mal-data]
  (def parens
    {:list ["(" ")"]
     :vector ["[" "]"]
     :hash-map ["{" "}"]})
  (def ss (map pr_str (mal-data :value)))
  (def p (parens (mal-data :type)))
  (string (p 0) (string/join ss " ") (p 1)))

(set pr_str
     (fn [mal-data]
       (if-not (= :struct (type mal-data))
         (break (string mal-data)))

       (case (mal-data :type)
         :list (pr_list mal-data)
         :vector (pr_list mal-data)
         :hash-map (pr_list mal-data)
         :comment ""
         :nil "nil"
         (string mal-data))
       ))

