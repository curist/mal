(defn pr_str [mal-data]
  (case (type mal-data)
    :array (do
             (def ss (map pr_str mal-data))
             (string "(" (string/join ss " ") ")"))
    (string mal-data)))
