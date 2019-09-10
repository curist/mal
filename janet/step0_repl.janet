(defn READ [s] s)
(defn EVAL [s] s)
(defn PRINT [s] s)

(defn rep [line]
  (-> line
      READ
      EVAL
      PRINT))

(defn puts [s]
  (:write stdout s)
  (:flush stdout))

(defn main []
  (while true
    (puts "user> ")
    (def line (getline))
    (if (empty? line) (break))
    (puts (rep line))))

(main)
