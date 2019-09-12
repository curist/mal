(defn READ [s] s)
(defn EVAL [s] s)
(defn PRINT [s] s)

(defn rep [line]
  (-> line
      READ
      EVAL
      PRINT))

(while true
  (:write stdout "user> ")
  (def line (:read stdin :line))
  (if (nil? line) (break))
  (:write stdout (rep line)))

