(defn pr_str [mal-data]
  (case (type mal-data)
    :struct
    (do
      (def parens
        {:list ["(" ")"]
         :vector ["[" "]"]
         :hash-map ["{" "}"]})
      (def ss (map pr_str (mal-data :value)))
      (def p (parens (mal-data :type)))
      (string (p 0) (string/join ss " ") (p 1)))
    (string mal-data)))
