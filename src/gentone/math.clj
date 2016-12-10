(ns gentone.math
  (:require [gentone.util :refer :all]))

(defn half
  [x]
  (/ x 2))

(defn soft-long
  [x]
  (if (== (long x) x) (long x) x))

(defn dist
  [x y]
  (double (Math/abs (- y x))))

(defn pow
  [a x]
  (soft-long (Math/pow a x)))

(defn exp
  [x]
  (pow Math/E x))

(defn log
  [x]
  (Math/log x))

(def log-of-2 (log 2))

(defn log2
  [x]
  (soft-long (/ (log x) log-of-2)))

(defn differences
  [v]
  (let [u (rotate-left v)]
    (map - u v)))

(defn ratios
  [v]
  (let [u (rotate-left v)]
    (map / u v)))

(defn sums
  ([v] (sums 0 v))
  ([x v] (drop-last 1 (reductions + x v))))

(defn products
  ([v] (products 1 v))
  ([x v] (drop-last 1 (reductions * x v))))

(defn entropy-from-probabilities
  [p]
  (double (- (reduce + (map #(* % (log2 %)) p)))))

(defn entropy
  [v]
  (let [c  (count v)
        f  (frequencies v)
        p  (map #(/ % c) (vals f))]
    (entropy-from-probabilities p)))

(defn gaussian
  [sigma x]
  (exp (/ (* x x) (* -2 sigma sigma))))

(defn gaussian-scaled-entropy
  [sigma v]
  (let [f  (frequencies v)
        p  (map #(* (get f %) (gaussian sigma %)) (keys f))
        s  (reduce + p)
        p2 (map #(/ % s) p)]
    (entropy-from-probabilities p2)))
