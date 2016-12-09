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
