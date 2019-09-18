(def Reader
  @{:tokens @[]
    :pos 0
    :next (fn [self]
            (def t (:peek self))
            (if (nil? (t :tok)) (error "no more tokens"))
            (update self :pos |(+ $ 2))
            t)
    :peek (fn [self]
            {:type (get-in self [:tokens (self :pos)])
             :tok (get-in self [:tokens (inc (self :pos))])})})

# [\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*)

# [\s,]*: Matches any number of whitespaces or commas. This is not captured so it will be ignored and not tokenized.
#
# ~@: Captures the special two-characters ~@ (tokenized).
#
# [\[\]{}()'`~^@]: Captures any special single character, one of []{}()'`~^@ (tokenized).
#
# "(?:\\.|[^\\"])*"?: Starts capturing at a double-quote and stops at the next double-quote unless it was preceded by a backslash in which case it includes it until the next double-quote (tokenized). It will also match unbalanced strings (no ending double-quote) which should be reported as an error.
#
# ;.*: Captures any sequence of characters starting with ; (tokenized).
#
# [^\s\[\]{}('"`,;)]*: Captures a sequence of zero or more non special characters (e.g. symbols, numbers, "true", "false", and "nil") and is sort of the inverse of the one above that captures special characters (tokenized).

(defn t
  "capture and tag a pattern"
  [pat &opt tag]
  (default tag pat)
  ~(<- (* ,pat (constant ,tag))))

(def mal-token
  ~{:whitespace (set " \t\n\r")
    :dontcare (+ :whitespace ",")
    :special-double (* "~@")
    :open-paren (set "([{")
    :special-single (set "[]{}()'`~^@")
    :maybe-string (* "\""
                     (any (+ (* "\\" 1)
                             (if-not (set "\\\"") 1)))
                     (? "\""))
    :comment (* ";" (any 1))
    :common (any (if-not (+ (set "[]{}('\"`,;)")
                            :whitespace)
                   1))
    :value (* (any :dontcare)
              (any (+ ,(t :special-double)
                      ,(t :open-paren)
                      ,(t :special-single)
                      ,(t :maybe-string)
                      ,(t :comment)
                      ,(t :common)
                      )))
    :main (any :value)
    })

(def mal-grammer (peg/compile mal-token))

(defn make-reader [tokens]
  (table/setproto @{:tokens tokens} Reader))

(defn tokenize [s]
  (peg/match mal-grammer s))

(var read_form nil)

(def parens
  {"(" ")"
   "[" "]"
   "{" "}"})

(defn read_list [reader]
  (def result @[])
  (def t (:next reader))
  (def p (parens (t :tok)))
  (loop [t :iterate (read_form reader) :until (= p t)]
    (array/push result t))
  {:type (case p
           ")" :list
           "]" :vector
           "}" :hash-map)
   :value result})

(defn read_atom [reader]
  (def t (:next reader))
  (t :tok))

(set read_form
     (fn read_form [reader]
       (def tok (:peek reader))
       (if (nil? (tok :tok)) (error 'EOF))
       (match tok
              {:type :comment} (:next reader)
              {:type :open-paren} (read_list reader)
              (read_atom reader))
       ))

(defn read_str [s]
  (def reader (-> s tokenize make-reader))
  (try
    (read_form reader)
    ([err] err)))


