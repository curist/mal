(use ./core/reader ./core/printer)
(import ./core/env :as env)

(defn READ [s] (read_str s))
(var EVAL nil)
(defn eval_ast [ast env]
  (match (ast :kind)
         :symbol (let [sym (:get env (ast :value))]
                   (if-not (nil? sym)
                     (break sym))
                   (error
                     {:type :error
                      :message (string (ast :value) " not found")}))
         :list (map |(EVAL $ env) (ast :value))
         :vector {:type :list
                  :kind :vector
                  :value (map |(EVAL $ env) (ast :value))}
         :hash-map {:type :list
                    :kind :hash-map
                    :value (map |(EVAL $ env) (ast :value))}
         :number {:type :atom :value (scan-number (ast :value))}
         ast
         ))

(set EVAL
     (fn EVAL [ast env]
       (if-not (= :list (ast :type))
         (break (eval_ast ast env)))
       (if (empty? (ast :value)) (break ast))
       (if-not (= :list (ast :kind))
         (break (eval_ast ast env)))

       (match (get-in ast [:value 0 :value])
              "def!" (let [[_ malk malv] (ast :value)
                           v (EVAL malv env)]
                       (:set env (malk :value) v))
              "let*" (let [env* (env/make-mal-env env)
                           mal-bindings (get-in ast [:value 1 :value])
                           mal-exp (get-in ast [:value 2])]
                       (if-not (even? (length mal-bindings))
                         (error {:type :error
                                 :message "let* bindings should be in pairs"}))
                       (loop [[malk malv] :in (partition 2 mal-bindings)]
                         (def v (EVAL malv env*))
                         (:set env* (malk :value) v))
                       (EVAL mal-exp env*))
              "do" (let [exprs (drop 1 (ast :value))]
                     (reduce (fn [_ mal-exp]
                               (EVAL mal-exp env))
                             nil exprs))

              # invoke as normal function
              (do
                (def result (eval_ast ast env))
                (def maybe-fn (first result))
                (if-not (= :fn (maybe-fn :type))
                  (error {:type :error
                          :message (string (maybe-fn :value) " is not invokable")}))
                (def f (maybe-fn :value))
                (def args (map |($ :value) (drop 1 result)))
                {:type :atom
                 :value (apply f args)}))))

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
  (def repl-env (env/make-mal-env))

  (:set repl-env "+" {:type :fn :value +})
  (:set repl-env "-" {:type :fn :value -})
  (:set repl-env "*" {:type :fn :value *})
  (:set repl-env "/" {:type :fn :value (fn [a b] (math/floor (/ a b)))})

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

