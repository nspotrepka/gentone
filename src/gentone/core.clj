(ns gentone.core
  (:require [pink.simple       :refer :all]
            [gentone.sequencer :refer :all]
            [gentone.sound     :refer :all]))

(defn run-generation
  "Run sequence generation."
  [{:keys [duration frequency
           root-size iterations migrations
           impulse-count rest-count
           pitches p1 p2 durations d1 d2
           sigma-p1 sigma-p2 sigma-d1 sigma-d2]
    :or   {duration 1 frequency 440
           root-size 8 iterations 8 migrations 24
           impulse-count 8 rest-count 0
           pitches 4 p1 0 p2 0 durations 4 d1 0 d2 0
           sigma-p1 1000 sigma-p2 1000 sigma-d1 1000 sigma-d2 1000}}]
  (let [s (generate-sequence
            root-size
            iterations
            migrations
            impulse-count
            rest-count
            pitches p1 p2
            durations d1 d2
            sigma-p1 sigma-p2 sigma-d1 sigma-d2)
        f (future
            (start-engine)
            (Thread/sleep 20)
            (println)
            (play-sequence-twice duration frequency s)
            (Thread/sleep (* duration 2000))
            (clear-engine)
            (stop-engine))]
    s))

(defn foo
  "An example function."
  []
  (run-generation
    {:duration 1 :frequency 440
     :root-size 8 :iterations 8 :migrations 24
     :impulse-count 8 :rest-count 0
     :pitches 4 :p1 0 :p2 0 :durations 4 :d1 0 :d2 0
     :sigma-p1 1000 :sigma-p2 1000 :sigma-d1 1000 :sigma-d2 1000}))
