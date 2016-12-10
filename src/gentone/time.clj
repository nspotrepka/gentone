(ns gentone.time
  (:require [gentone.util :refer :all]
            [gentone.math :refer :all]))

(defn times
  [s]
  (map :time s))

(defn times-r
  [s]
  (let [tt    (times s)
        rests (map :rest s)]
    (filter-rests-echo tt rests)))

(defn time-duration
  [v]
  (map #(if (> % 1) 1 (mod % 1)) (differences v)))
