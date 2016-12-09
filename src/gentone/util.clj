(ns gentone.util)

(defn rotate-left
  ([v] (rotate-left 1 v))
  ([n v] (concat (drop n v) (take n v))))

(defn rotate-right
  ([v] (rotate-right 1 v))
  ([n v] (rotate-left (- (count v) n) v)))

(defn echo-rests
  [rr]
  (map #(and %1 %2) rr (rotate-right rr)))

(defn filter-rests
  [v rr]
  (let [echo       (echo-rests rr)
        with-rests (map (fn [x y] [x y]) v echo)]
    (map first (filter (comp not second) with-rests))))

(defn rests-r
  [s]
  (let [rests (map :rest s)]
    (filter-rests rests rests)))
