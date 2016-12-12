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

(defn dist2
  [x y]
  (let [d (dist x y)]
    (* d d)))

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

(defn normalize
  [v]
  (let [s (reduce + v)]
    (if (== 0.0 s)
      v
      (map #(/ % s) v))))

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

(defn entropy-from-distribution
  [p]
  (double (- (reduce + (map #(* % (log2 %)) p)))))

(defn entropy-from-probabilities
  [p]
  (let [c (count p)]
    (if (= c 0)
      0.0
      (double (/ (- (reduce + (map log2 p))) c)))))

(defn entropy
  [v]
  (let [c  (count v)
        f  (frequencies v)
        p  (map #(/ % c) (vals f))]
    (entropy-from-distribution p)))

(defn gaussian
  [sigma x]
  (exp (/ (* x x) (* -2 sigma sigma))))

(defn gaussian-entropy
  [sigma v]
  (let [f (frequencies v)
        a (map #(* (get f %) (gaussian sigma %)) (keys f))
        p (normalize a)]
    (entropy-from-distribution p)))

(defn distribution-entropy
  [f v]
  (entropy-from-probabilities (map #(if-let [x (get f %)] x 0.000001) v)))
