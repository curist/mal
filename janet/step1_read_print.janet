(use ./reader ./printer)

(defn READ [s] (read_str s))
(defn EVAL [s] s)
(defn PRINT [s] (pr_str s))

(defn rep [line]
  (try
    (-> line READ EVAL PRINT)
    ([err]
     (match err
            {:type :error} (print (err :kind))
            (error err)))))

(defn main [_]
  (while true
    (:write stdout "user> ")
    (def line (:read stdin :line))
    (if (nil? line) (break))
    (def resp (rep line))
    (if-not (nil? resp) (print resp))))

