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

(defn info-from-distribution
  [p]
  (double (- (reduce + (map #(* % (log2 %)) p)))))

(defn info-from-probabilities
  [p]
  (let [c (count p)]
    (if (= c 0)
      0.0
      (double (/ (- (reduce + (map log2 p))) c)))))

(defn info-from-distribution-scaled
  [p q]
  (double (- (reduce + (map #(* %1 (log2 %2)) p (map * p q))))))

(defn info
  [v]
  (let [c  (count v)
        f  (frequencies v)
        p  (map #(/ % c) (vals f))]
    (info-from-distribution p)))

(defn gaussian
  [sigma x]
  (exp (/ (* x x) (* -2 sigma sigma))))

(defn gaussian-info
  [sigma v]
  (let [c (count v)
        f (frequencies v)
        p (map #(/ % c) (vals f))
        q (map (partial gaussian sigma) (keys f))]
    (info-from-distribution-scaled p q)))

(defn distribution-info
  [f v]
  (info-from-probabilities (map #(if-let [x (get f %)] x 0) v)))
