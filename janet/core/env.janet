(def Env
  @{:data @{}
    :set (fn [self k v]
           (update self :data |(put $ k v))
           v)
    :find (fn [self k]
            (get-in self [:data k]
                    (if (nil? (self :outer))
                      nil
                      (:find (self :outer) k))))
    :get (fn [self k]
           (def v (:find self k))
           (if (nil? v)
             (error {:type :error
                     :message (string k " not found")})
             v))})

(defn make-mal-env [&opt outer]
  (default outer nil)
  (table/setproto @{:outer outer} Env))
