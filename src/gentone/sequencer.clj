(ns gentone.sequencer
  (:require [fungp.core    :refer :all])
  (:require [gentone.math  :refer :all])
  (:require [gentone.pitch :refer :all])
  (:require [gentone.time  :refer :all]))

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

;; EDIT THIS FITNESS FUNCTION

(defn make-fitness
  []
  (fn [tree]
    (let [s (eval-tree tree)
          p (pitches s)
          d (durations s)]
      (- 16 (count (flatten tree))))))

(defn printer
  [tree fitness]
  (let [s (eval-tree tree)]
    (println tree)
    (println (pitches s))
    (println (times s))
    (println (str "Error: " fitness "\n"))
    (flush)))

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
    tree))
