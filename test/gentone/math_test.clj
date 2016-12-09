(ns gentone.math_test
  (:require [clojure.test :refer :all]
            [gentone.math :refer :all]))

(deftest rotate-left-test
  (testing "rotate-left"
    (is (= [] (rotate-left [])))
    (is (= [2 3 4 1] (rotate-left [1 2 3 4])))
    (is (= [8 7 6 9] (rotate-left [9 8 7 6])))
    (is (= [2 3 4 5 0 1] (rotate-left 2 [0 1 2 3 4 5])))
    (is (= [5 0 1 2 3 4] (rotate-left 5 [0 1 2 3 4 5])))))

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
