(ns gentone.core
  (:require [pink.simple       :refer :all]
            [gentone.math      :refer :all]
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
           sigma-p1 2 sigma-p2 4 sigma-d1 2 sigma-d2 4}}]
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

(defn simple-rhythm
  "Simple sequence with n impulses, calculated with m migrations."
  [n m]
  (run-generation
    {:duration 1 :frequency 440
     :root-size 8 :iterations 8 :migrations m
     :impulse-count n :rest-count 0
     :pitches 1 :p1 0 :p2 0 :durations 1 :d1 0 :d2 0
     :sigma-p1 2 :sigma-p2 4 :sigma-d1 2 :sigma-d2 4}))

(defn exponential-rhythm
  "Simple sequence with n impulses, calculated with m migrations."
  [n m]
  (run-generation
    {:duration 1 :frequency 440
     :root-size 8 :iterations 8 :migrations m
     :impulse-count n :rest-count 0
     :pitches 1 :p1 0 :p2 0 :durations (dec n) :d1 1/2 :d2 0
     :sigma-p1 2 :sigma-p2 4 :sigma-d1 2 :sigma-d2 4}))

(defn simple-tune
  "Simple sequence with n pitches, r rests, p pitches, d durations,
   and pitch/duration movement of 2, calculated with m migrations."
  [n r p d m]
  (run-generation
    {:duration 1 :frequency 440
     :root-size 8 :iterations 8 :migrations m
     :impulse-count n :rest-count r
     :pitches p :p1 2 :p2 0 :durations d :d1 2 :d2 0
     :sigma-p1 2 :sigma-p2 4 :sigma-d1 2 :sigma-d2 4}))
