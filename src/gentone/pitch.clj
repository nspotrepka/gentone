(ns gentone.pitch
  (:require [gentone.math :refer :all]))

(defn pitches
  [s]
  (map :pitch s))

(defn rests
  [s]
  (map :rest s))

(defn pitches-r
  [s]
  (let [last-pitch (->> s (reverse) (drop-while :rest) (first) (:pitch))]
    (rotate-left
      (reductions
        #(if (:rest %2) %1 (:pitch %2))
        last-pitch
        (drop-last 1 s)))))
