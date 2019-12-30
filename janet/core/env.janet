(def mal-env
  @{"+" +
    "-" -
    "*" *
    "/" (fn [a b] (math/floor (/ a b)))})
