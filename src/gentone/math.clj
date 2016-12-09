(ns gentone.math)

(defn half
  [x]
  (/ x 2))

(defn rotate-left
  ([v] (rotate-left 1 v))
  ([n v] (concat (drop n v) (take n v))))

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
