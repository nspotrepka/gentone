(ns gentone.loop
  (:require [fungp.core     :refer :all]
            [clojure.pprint :refer :all]))

(def log? true)

(def default-note {:start  0
                   :length 1
                   :pitch  0
                   :amp    1})

(def terminals `[nil [default-note]])

(defn pitch-down
  [v]
  (map #(update % :pitch dec) v))

(defn pitch-up
  [v]
  (map #(update % :pitch inc) v))

(defn move-left
  [v]
  (map #(update % :start dec) v))

(defn move-right
  [v]
  (map #(update % :start inc) v))

(defn half-length
  [v]
  (let [half #(/ % 2)]
    (map #(update (update % :start half) :length half) v)))

(defn combine
  [v0 v1]
  (loop [[xf & xr :as x] (if (nil? v0) '() v0)
         [yf & yr :as y] (if (nil? v1) '() v1)
         r []]
    (if (or (empty? x) (empty? y))
      (concat r x y)
      (if (<= (:start xf) (:start yf))
        (recur xr y (conj r xf))
        (recur x yr (conj r yf))))))

(def functions
  `[[pitch-down  1]
    [pitch-up    1]
    [move-left   1]
    [move-right  1]
    [half-length 1]
    [combine     2]])

(defn make-fitness
  []
  (fn [tree]
    (let [l (list 'fn '[] tree)
          f (eval l)
          v (f)
          num (reduce + (map :pitch v))]
      (if (= num 0)
        1000
        num))))

(defn report
  [tree fitness]
  (when log?
    (let [l (list 'fn '[] tree)
          f (eval l)
          v (f)]
      (pprint l)
      (pprint v)
      (print (str "Error:\t" fitness "\n\n"))
      (flush))))

(defn make-loop
  [n1 n2]
  (println "Motif Generation through GP")
  (let [fitness (make-fitness)
        options {:iterations n1
                 :migrations n2
                 :num-islands 5
                 :population-size 25
                 :terminals terminals
                 :tournament-size 4
                 :mutation-probability 0.16
                 :functions functions
                 :fitness fitness
                 :report report
                 :max-depth 8
                 :mutation-depth 5
                 :adf-count 0
                 :adf-arity 1}
        [tree score] (rest (run-genetic-programming options))]
    (do (println "Done!")
        (report tree score))))
