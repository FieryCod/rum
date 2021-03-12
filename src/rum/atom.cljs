(ns ^:no-doc rum.atom
  (:refer-clojure :exclude [->Atom Atom atom derive]))

;; https://github.com/funcool/okulary/blob/master/src/okulary/core.cljs
(deftype Atom [state watches]
  Object
  (equiv [self other]
    (-equiv self other))

  IAtom
  IEquiv
  (-equiv [o other] (identical? o other))

  IDeref
  (-deref [_] state)

  IReset
  (-reset! [self newval]
    (let [oldval state]
      (set! (.-state self) newval)
      (when (> (.-size watches) 0)
        (-notify-watches self oldval newval))
      newval))

  ISwap
  (-swap! [self f]
    (-reset! self (f state)))
  (-swap! [self f x]
    (-reset! self (f state x)))
  (-swap! [self f x y]
    (-reset! self (f state x y)))
  (-swap! [self f x y more]
    (-reset! self (apply f state x y more)))

  IWatchable
  (-notify-watches [self oldval newval]
    (let [a (js/Array.from (.entries watches))
          t (alength a)]
      (loop [i 0]
        (when (< i t)
          (let [nx (aget a i)
                f  (aget nx 1)
                k  (aget nx 0)]
            (f k self oldval newval)
            (recur (inc i)))))))

  (-add-watch [self key f]
    (.set watches key f)
    self)

  (-remove-watch [self key]
    (.delete watches key))

  IHash
  (-hash [self] (goog/getUid self)))

(defn atom
  "Creates and returns an Atom with an initial value of x."
  [x]
  (Atom. x (js/Map.)))

(def EMPTY (js/Symbol "empty"))

(deftype DerivedAtom [id f sources equals? watchers srccache cache]
  IAtom
  IDeref
  (-deref [self]
    (let [sources' (.map sources deref)]
      (if (and (not (identical? cache EMPTY))
               (.every sources' (fn [v i] (identical? v (aget srccache i)))))
        (.-cache self)
        (let [result (case (.-length sources')
                       1 (f (aget sources' 0))
                       2 (f (aget sources' 0) (aget sources' 1))
                       3 (f (aget sources' 0) (aget sources' 1) (aget sources' 3))
                       (.apply f nil sources'))]
          (set! (.-srccache self) sources')
          (set! (.-cache self) result)
          result))))

  IWatchable
  (-add-watch [self key cb]
    (.set watchers key cb)
    (when (identical? (.-size watchers) 1)
      (let [sources' (mapv deref sources)
            result   (case (count sources')
                       1 (f (aget sources' 0))
                       2 (f (aget sources' 0) (aget sources' 1))
                       3 (f (aget sources' 0) (aget sources' 1) (aget sources' 2))
                       (.apply f sources'))
            watch-fn (fn [_ _ old-val new-val]
                       (when-not (identical? old-val new-val)
                         (set! (.-srcscache self) sources')
                         (set! (.-cache self) result)
                         (when-not ^boolean (equals? result cache)
                           (let [a (js/Array.from (.entries watchers))
                                 t (alength a)]
                             (loop [i 0]
                               (when (< i t)
                                 (let [nx (aget a i)
                                       f  (aget nx 1)
                                       k  (aget nx 0)]
                                   (f k self cache result)
                                   (recur (inc i)))))))))]
        (doseq [source sources]
          (add-watch source id watch-fn))))
    self)

  (-remove-watch [self key]
    (.delete watchers key)
    (when (identical? (.-size watchers) 0)
      (doseq [source sources] (remove-watch source id))
      (set! (.-cache self) EMPTY))))

(defn derived-atom
  "Create a derived atom from an other atoms with the provided lense.

  The returned atom is lazy, so no code is executed until user
  requires it.

  By default the reaction atom does not trigger updates if the data
  does not affects to it (determined by selector), but this behavior
  can be deactivated passing `:equals?` to `false` on the third
  options parameter. You also may pass `=` as `equals?` parameter if
  you want value comparison instead of reference comparison with
  `identical?`."
  ([sources selector]
   (derived-atom sources selector identical?))
  ([sources selector equals?]
   (DerivedAtom. (js/Symbol "derived-atom") selector (into-array nil sources) equals? (js/Map.) EMPTY
                  EMPTY)))
