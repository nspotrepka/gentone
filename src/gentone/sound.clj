(ns gentone.sound
  (:require [pink.simple :refer :all]
            [pink.event :refer :all]
            [pink.envelopes :refer [hold xar]]
            [pink.filters :refer [butterlp]]
            [pink.oscillators :refer [blit-saw]]
            [pink.space :refer [pan]]
            [pink.util :refer [mul sum]]
            [gentone.math :refer :all]
            [gentone.pitch :refer :all]
            [gentone.time :refer :all]
            [gentone.util :refer :all]
            [gentone.sequencer :refer :all]))

(defn filtered-saw
  [dur amp freq]
  (pan
    (mul
      (butterlp
        (blit-saw freq)
        (sum 300 (mul 7700 (xar 0.02 (- dur 0.02)))))
      (hold amp dur))
    0.0))

(defn play-sequence
  [dur freq s]
  (let [hash       (reduce #(+ %1 (int %2)) 0 (str s))
        [tt pp rr] (split-sequence s)
        dd         (time-duration tt)
        m          (comp soft-long (partial * dur))
        ts         (map m tt)
        ds         (map m dd)
        pf         (pitch-freq freq pp)]
    (println "hash\t" hash)
    (println "dur\t" dur)
    (println "freq\t" freq)
    (println "times\t" ts)
    (println "pitches\t" pp)
    (println "rests\t" rr)
    (let [evts (filter-rests
                 (concat
                   (map #(i filtered-saw %1 %2 %2 0.5 %3) ts ds pf)
                   (map #(i filtered-saw (+ dur %1) %2 %2 0.5 %3) ts ds pf))
                 (concat rr rr))]
      (when-not (empty? evts)
        (apply add-audio-events evts)))))

;; TODO: Move this to initialization function
(start-engine)
