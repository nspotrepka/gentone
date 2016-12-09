(ns gentone.time
  (:require [gentone.util :refer :all]
            [gentone.math :refer :all]))

(defn times
  [s]
  (map :time s))

(defn times-r
  [s]
  (let [rests (map :rest s)]
    (filter-rests (times s) rests)))

(defn time-duration
  [v]
  (map #(mod % 1) (differences v)))
