(ns gentone.sequencer
  (:require [fungp.core    :refer :all]
            [gentone.util  :refer :all]
            [gentone.math  :refer :all]
            [gentone.pitch :refer :all]
            [gentone.time  :refer :all]))

(def default-note {:time  0
                   :step  1
                   :pitch 0
                   :rest  false})

(def terminals `[[default-note]])

(defn make-sequence
  ([tt pp]
   (make-sequence tt pp (repeat (count pp) false)))
  ([tt pp rr]
   (map
     (fn [t p r] {:time t :pitch p :rest r})
     tt
     pp
     rr)))

(defn clean-sequence
  [s]
  (let [s1 (sort-by :time s)
        s2 (filter-rests-echo s1 (map :rest s1))
        s3 (concat (drop-while :rest s2) (take-while :rest s2))
        f  (first s3)
        t  (:time f)
        p  (:pitch f)]
    (map
      (fn [m]
        (-> m
          (dissoc :step)
          (update :time #(mod (- % t) 1))
          (update :pitch #(- % p))))
      s3)))

(defn split-sequence
  [s]
  (let [tt (times-r s)
        pp (pitches-r s)
        rr (rests-r s)]
    [tt pp rr]))

(defn pitch-down
  [v]
  (map #(update % :pitch dec) v))

(defn pitch-up
  [v]
  (map #(update % :pitch inc) v))

(defn pitch-down-down
  [v]
  (map #(update % :pitch (comp dec dec)) v))

(defn pitch-up-up
  [v]
  (map #(update % :pitch (comp inc inc)) v))

(defn toggle-rest
  [v]
  (map #(update % :rest not) v))

(defn half-left
  [v]
  (map #(let [h (half (:step %))]
         (-> %
           (update :step (constantly h))
           (update :time (fn [x] (- x h)))))
    v))

(defn half-right
  [v]
  (map #(let [h (half (:step %))]
         (-> %
           (update :step (constantly h))
           (update :time (fn [x] (+ x h)))))
    v))

(defn double-left
  [v]
  (map #(let [s (:step %)]
         (-> %
           (update :step (fn [x] (* 2 x)))
           (update :time (fn [x] (- x s)))))
    v))

(defn double-right
  [v]
  (map #(let [s (:step %)]
         (-> %
           (update :step (fn [x] (* 2 x)))
           (update :time (fn [x] (+ x s)))))
    v))

(defn make-functions
  [n-concat]
  (into
    [[`pitch-down      1]
     [`pitch-up        1]
     [`pitch-down-down 1]
     [`pitch-up-up     1]
     [`toggle-rest     1]
     [`half-left       1]
     [`half-right      1]
     [`double-left     1]
     [`double-right    1]]
    (repeat n-concat [concat 2])))

(defn loop-sequence
  [v]
  (map #(update % :time (fn [x] (mod x 1))) v))

(defn eval-tree
  [tree]
  (let [l (list 'fn '[] tree)
        f (eval l)]
    (clean-sequence (loop-sequence (f)))))

;; WORKSPACE BEGIN

(defn make-fitness
  "Toying around with fitness."
  [c r p p1 p2 d d1 d2 sp1 sp2 sd1 sd2]
  (fn [tree]
    (let [s          (eval-tree tree)
          [tt pp rr] (split-sequence s)]
      (if (not= tt (distinct tt))
        1000000
        (let [pp2        (filter-rests pp rr)
              dd         (map log2 (time-duration tt))
              p-delta    (differences pp2)
              d-delta    (differences dd)
              p-delta2   (differences p-delta)
              d-delta2   (differences d-delta)
              pp-e       (info pp2)
              dd-e       (info dd)
              rr-e       (info rr)
              p-delta-e  (gaussian-info sp1 p-delta)
              p-delta2-e (gaussian-info sp2 p-delta2)
              d-delta-e  (gaussian-info sd1 d-delta)
              d-delta2-e (gaussian-info sd2 d-delta2)]
          (+
            (dist2 c (count tt))
            (dist2 r (count (filter identity rr)))
            (dist2 (log2 p) pp-e)
            (* 1/2 (dist2 p1 p-delta-e))
            (* 1/4 (dist2 p2 p-delta2-e))
            (dist2 (log2 d) dd-e)
            (* 1/2 (dist2 d1 d-delta-e))
            (* 1/4 (dist2 d2 d-delta2-e))
            ))))))

(defn printer
  [tree fitness]
  (let [s (eval-tree tree)]
    (println "id\t" (reduce #(+ %1 (int %2)) 0 (str s)))
    (println "times\t" (times s))
    (println "pitches\t" (pitches s))
    (println "rests\t" (map :rest s))
    (println "error\t" fitness "\n")
    (flush)))

;; WORKSPACE END

(defn generate-sequence
  [root-size iterations migrations c r p p1 p2 d d1 d2 sp1 sp2 sd1 sd2]
  (let [functions    (make-functions 3)
        fitness      (make-fitness c r p p1 p2 d d1 d2 sp1 sp2 sd1 sd2)
        options      {:terminals terminals
                      :functions functions
                      :fitness fitness
                      :report printer
                      :iterations iterations
                      :migrations migrations
                      :num-islands root-size
                      :population-size root-size
                      :tournament-size (int (+ 1 (log2 root-size)))
                      :mutation-probability 0.4
                      :max-depth 24
                      :mutation-depth 16
                      :adf-count 0
                      :adf-arity 1}
        gp           (run-genetic-programming options)
        collection   (first gp)
        [tree score] (rest gp)]
    (println "score\t" score "\n")
    (eval-tree tree)))
