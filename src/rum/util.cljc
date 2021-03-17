(ns ^:no-doc rum.util)

(defn collect
  [key mixins]
  (persistent!
   (reduce
    (fn [acc m]
      (if-let [v (.get m key)]
        (conj! acc v)
        acc))
    (transient [])
    mixins)))

(defn collect*
  [keys mixins]
  (persistent!
   (reduce
    (fn [acc m]
      (reduce
       (fn [acc k]
         (if-let [v (.get m k)]
           (conj! acc v)
           acc))
       acc
       keys))
    (transient [])
    mixins)))

(defn call-fns
  ([state fns             ] (reduce (fn [acc f] (f acc                 )) state fns))
  ([state fns x           ] (reduce (fn [acc f] (f acc x               )) state fns))
  ([state fns x y         ] (reduce (fn [acc f] (f acc x y             )) state fns))
  ([state fns x y z       ] (reduce (fn [acc f] (f acc x y z           )) state fns))
  ([state fns x y z & args] (reduce (fn [acc f] (apply f acc x y z args)) state fns)))

(defn into-all
  "Like `into` but supports multiple \"from\"s.
  ~3-5x faster than concat and return vector, not lazy-seq"
  ([to from       ] (into to from))
  ([to from & more]
   (persistent!
    (reduce (fn [acc in] (reduce conj! acc in))
            (transient to)
            (cons from more)))))
