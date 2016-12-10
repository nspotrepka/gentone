(ns gentone.math
  (:require [gentone.util :refer :all]))

(defn half
  [x]
  (/ x 2))

(defn soft-long
  [x]
  (if (== (long x) x) (long x) x))

(defn pow
  [a x]
  (soft-long (Math/pow a x)))

(defn log
  [x]
  (Math/log x))

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

(defn gaussian-info
  [sigma v]
  (let [a  (* 2 sigma sigma)
        b  (* (count v) (+ (log sigma) (half (log (* 2 Math/PI)))))
        xx (reduce + (map #(* % %) v))]
    (+ b (/ xx a))))
