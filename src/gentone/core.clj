(ns gentone.core
  (:require [pink.simple       :refer :all]
            [gentone.sequencer :refer :all]
            [gentone.sound     :refer :all]))

(defn foo
  "An example function."
  [dur freq i m]
  (let [s (generate-sequence i m)
        f (future
            (start-engine)
            (Thread/sleep 20)
            (play-sequence dur freq s)
            (Thread/sleep (* dur 2000))
            (clear-engine)
            (stop-engine))]
    (clean-sequence s)))
