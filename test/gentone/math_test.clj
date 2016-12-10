(ns gentone.math_test
  (:require [clojure.test :refer :all]
            [gentone.math :refer :all]))

(deftest half-test
  (testing "half"
    (is (= 0 (half 0)))
    (is (= 1/2 (half 1)))
    (is (= 1 (half 2)))))

(deftest soft-long-test
  (testing "soft-long"
    (is (= 0 (soft-long 0)))
    (is (= 1 (soft-long 1.0)))
    (is (= 1.1 (soft-long 1.1)))))

(deftest pow-test
  (testing "pow"
    (is (= 0 (pow 0 1)))
    (is (= 1 (pow 1 0)))
    (is (= 2 (pow 2 1)))
    (is (= 1.5 (pow 2.25 1/2)))))

(deftest log-test
  (testing "log"
    (is (= 0.0 (log 1)))
    (is (= 1.0 (log Math/E)))
    (is (= 2.0 (log (* Math/E Math/E))))))

(deftest differences-test
  (testing "differences"
    (is (= [] (differences [])))
    (is (= [0 0 0 0] (differences [1 1 1 1])))
    (is (= [1 5 -2 -4] (differences [1 2 7 5])))))

(deftest ratios-test
  (testing "differences"
    (is (= [] (ratios [])))
    (is (= [1 1 1 1] (ratios [7 7 7 7])))
    (is (= [2 7/2 5/7 1/5] (ratios [1 2 7 5])))))

(deftest sums-test
  (testing "sums"
    (is (= [] (sums [])))
    (is (= [0 1 2 3] (sums [1 1 1 -3])))
    (is (= [4 7 4 5] (sums 4 [3 -3 1 -1])))))

(deftest products-test
  (testing "products"
    (is (= [] (products [])))
    (is (= [1 1/2 1 4] (products [1/2 2 4 1/4])))
    (is (= [5 4 8 1] (products 5 [4/5 2 1/8 5])))))

(deftest gaussian-info-test
  (testing "gaussian-info"
    (is
      (=
        (+ 0.5 (log (* 2 Math/PI)))
        (gaussian-info 1 [0 1])))
    (is
      (=
        (+ 0.5 (log (* 2 Math/PI)) (* 2 (log 2)))
        (gaussian-info 2 [0 2])))))
