(ns gentone.pitch
  (:require [gentone.util :refer :all]
            [gentone.math :refer :all]))

(defn pitches
  [s]
  (map :pitch s))

(defn pitches-r
  [s]
  (let [pp    (pitches s)
        rests (map :rest s)
        pp2   (filter-rests-echo pp rests)
        pp2r  (rotate-right pp2)]
    (switch-rests pp2 pp2r (rests-r s))))

(defn pitch-freq
  [f v]
  (map #(soft-long (* f (Math/pow 2 (/ % 12)))) v))
