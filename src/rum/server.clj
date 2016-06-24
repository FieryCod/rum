(ns rum.server
  (:require [rum.util :refer [next-id collect call-all]]))


(defn build-class [classes display-name]
  (assert (sequential? classes))
  (let [init             (collect :init classes)                ;; state props -> state
        will-mount       (collect :will-mount classes)          ;; state -> state
        render           (first (collect :render classes))      ;; state -> [dom state]
        wrapped-render   (reduce #(%2 %1) render (collect :wrap-render classes)) ;; render-fn -> render-fn
        props->state     (fn [props]
                           (call-all (:rum/initial-state props) init props))]

    (fn [props]
      (let [state       (-> {:rum/id (next-id)}
                            (merge (props->state props))
                            (call-all will-mount))
            [dom state] (wrapped-render state)]
        (or dom [::nothing])))))


(defn nothing? [element]
  (and (vector? element)
       (= ::nothing (first element))))


(defn args->state [args]
  {:rum/args args})


(defn element [class state & [props]]
  (class (assoc props :rum/initial-state state)))


(defn render->mixin [render-fn]
  { :render (fn [state] [(apply render-fn (:rum/args state)) state]) })


(defn render-state->mixin [render-fn]
  { :render (fn [state] [(apply render-fn state (:rum/args state)) state]) })


(defn render-comp->mixin [render-fn]
  { :render (fn [state] [(apply render-fn (:rum/react-component state) (:rum/args state)) state]) })


(defn with-key [element key]
  (cond
    (nothing? element)
    element
    
    (map? (get element 1))
    (assoc-in element [1 :key] key)

    :else
    (into [(first element) {:key key}] (next element))))


(defn with-ref [element ref]
  element)
