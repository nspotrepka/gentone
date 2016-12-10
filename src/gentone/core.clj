(ns gentone.core
  (:require [gentone.sequencer :refer :all]
            [gentone.sound     :refer :all]))

(defn foo
  "An example function."
  [dur freq i m]
  (play-sequence dur freq (generate-sequence i m)))
