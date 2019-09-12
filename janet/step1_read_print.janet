(use ./reader ./printer)

(defn READ [s] (read_str s))
(defn EVAL [s] s)
(defn PRINT [s] (pr_str s))

(defn rep [line]
  (-> line
      READ
      EVAL
      PRINT))

(defn main []
  (while true
    (:write stdout "user> ")
    (def line (:read stdin :line))
    (if (nil? line) (break))
    (print (rep line))))

(main)
