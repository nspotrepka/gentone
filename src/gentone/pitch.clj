(ns gentone.pitch
  (:require [gentone.util :refer :all]
            [gentone.math :refer :all]))

(defn pitches
  [s]
  (map :pitch s))

(defn pitches-r
  [s]
  (let [rests (map :rest s)]
    (filter-rests (pitches s) rests)))

(defn pitch-freq
  [f v]
  (map #(soft-long (* f (Math/pow 2 (/ % 12)))) v))
