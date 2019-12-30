(use ./core/reader ./core/printer)
(import ./core/env :as env)

(defn READ [s] (read_str s))
(var EVAL nil)
(defn eval_ast [ast env]
  (match (ast :kind)
         :symbol (let [sym (get env (ast :value))]
                   (if-not (nil? sym)
                     (break sym))
                   (error
                     {:type :error
                      :message (string (ast :value) " not found")}))
         :list (map |(EVAL $ env) (ast :value))
         :vector {:type :list
                  :kind :vector
                  :value (map |(EVAL $ env) (ast :value))}
         :hash-map ast
         :number {:type :atom :value (scan-number (ast :value))}
         ast
         ))

(set EVAL
     (fn EVAL [ast env]
       (if-not (= :list (ast :type))
         (break (eval_ast ast env)))
       (if (empty? (ast :value)) (break nil))
       (def result (eval_ast ast env))
       (if-not (= :list (ast :kind))
         (break result))
       (def f (first result))
       (def args (map |($ :value) (drop 1 result)))
       {:type :atom
        :value (apply f args)}))

(defn PRINT [exp] (pr_str exp))

(defn rep [line env]
  (try
    (-> line READ (EVAL env) PRINT)
    ([err]
     (match err
            {:type :error} (print (or (err :message) (err :kind)))
            {:type t} (print "unknown type " t)
            nil (print "unexpected nil error")
            (error err)))))

(defn make-repl []
  (def repl-env env/mal-env)
  (while true
    (:write stdout "user> ")
    (def line (:read stdin :line))
    (if (nil? line) (break))
    (def resp (rep line repl-env))
    (if-not (nil? resp) (print resp))
    (yield)))

(defn main [_]
  (def repl-fiber (fiber/new make-repl))
  (while (not= :dead (fiber/status repl-fiber))
    (resume repl-fiber)))

