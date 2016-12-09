(ns gentone.time
  (:require [gentone.math :refer :all]))

(defn times
  [s]
  (map :time s))

(defn durations
  [s]
  (map #(mod % 1) (differences (times s))))
