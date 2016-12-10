(ns gentone.sequencer
  (:require [fungp.core    :refer :all]
            [gentone.util  :refer :all]
            [gentone.math  :refer :all]
            [gentone.pitch :refer :all]
            [gentone.time  :refer :all]))

(def default-note {:time  0
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
  (let [s2 (filter-rests-echo s (map :rest s))
        s3 (concat (drop-while :rest s2) (take-while :rest s2))
        f  (first s3)
        t  (:time f)
        p  (:pitch f)]
    (map
      (fn [m]
        (-> m
          (update :time #(mod (- % t) 1))
          (update :pitch #(- % p))))
      s3)))

(defn split-sequence
  [s]
  (let [s2 (clean-sequence s)
        tt (times-r s2)
        pp (pitches-r s2)
        rr (rests-r s2)]
    [tt pp rr]))

(defn pitch-down
  [v]
  (map #(update % :pitch dec) v))

(defn pitch-up
  [v]
  (map #(update % :pitch inc) v))

(defn toggle-rest
  [v]
  (map #(update % :rest not) v))

(defn half-left
  [v]
  (map #(update % :time (comp half dec)) v))

(defn half-right
  [v]
  (map #(update % :time (comp half inc)) v))

(defn half-length
  [v]
  (map #(update % :time half) v))

(defn combine
  [v0 v1]
  (loop [[xf & xr :as x] (if (nil? v0) '() v0)
         [yf & yr :as y] (if (nil? v1) '() v1)
         r []]
    (if (or (empty? x) (empty? y))
      (concat r x y)
      (if (<= (:time xf) (:time yf))
        (recur xr y (conj r xf))
        (recur x yr (conj r yf))))))

(def functions
  `[[pitch-down  1]
    [pitch-up    1]
    [toggle-rest 1]
    [half-left   1]
    [half-right  1]
    [half-length 1]
    [combine     2]])

(defn loop-sequence
  [v]
  (let [neg (take-while (comp neg? :time) v)
        pos (drop-while (comp neg? :time) v)
        mod (map #(update % :time inc) neg)]
    (combine pos mod)))

(defn eval-tree
  [tree]
  (let [l (list 'fn '[] tree)
        f (eval l)]
    (loop-sequence (f))))

;; WORKSPACE BEGIN

(defn make-fitness
  "Toying around with fitness."
  []
  (fn [tree]
    (let [s (eval-tree tree)
          p (pitches-r s)
          t (times-r s)]
      (-
        (if (= t (distinct t)) -1 10000)
        (reduce #(+ %1 (Math/abs %2)) 0 (differences p))))))

(defn printer
  [tree fitness]
  (let [s (eval-tree tree)]
    (println "hash\t" (reduce #(+ %1 (int %2)) 0 (str s)))
    (println "times\t" (times s))
    (println "pitches\t" (pitches s))
    (println "rests\t" (map :rest s))
    (println "error\t" fitness "\n")
    (flush)))

;; WORKSPACE END

(defn generate-sequence
  [iterations migrations]
  (let [fitness (make-fitness)
        options {:terminals terminals
                 :functions functions
                 :fitness fitness
                 :report printer
                 :iterations iterations
                 :migrations migrations
                 :num-islands 4
                 :population-size 18
                 :tournament-size 4
                 :mutation-probability 0.1667
                 :max-depth 9
                 :mutation-depth 5
                 :adf-count 0
                 :adf-arity 1}
        [tree score] (rest (run-genetic-programming options))]
    (println "score\t" score "\n")
    (eval-tree tree)))
