(ns gentone.core
  (:require [gentone.sequencer :refer :all]))

(defn foo
  "An example function."
  []
  (generate-sequence 2 2))
