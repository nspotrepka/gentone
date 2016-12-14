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

(deftest dist-test
  (testing "dist"
    (is (= 1.0 (dist 0 1)))
    (is (= 1.0 (dist 1 0)))
    (is (= 2.2 (dist -1.1 1.1)))
    (is (= 3.0 (dist 1.5 -1.5)))))

(deftest dist2-test
  (testing "dist2"
    (is (= 1.0 (dist2 0 1)))
    (is (= 1.0 (dist2 1 0)))
    (is (= 4.0 (dist2 -1 1)))
    (is (= 9.0 (dist2 1.5 -1.5)))))

(deftest pow-test
  (testing "pow"
    (is (= 0 (pow 0 1)))
    (is (= 1 (pow 1 0)))
    (is (= 2 (pow 2 1)))
    (is (= 1.5 (pow 2.25 1/2)))))

(deftest exp-test
  (testing "exp"
    (is (= 1 (exp 0)))
    (is (= Math/E (exp 1)))
    (is (= (* Math/E Math/E) (exp 2)))
    (is (= (/ 1 Math/E) (exp -1)))))

(deftest log-test
  (testing "log"
    (is (= 0.0 (log 1)))
    (is (= 1.0 (log Math/E)))
    (is (= 2.0 (log (* Math/E Math/E))))))

(deftest log2-test
  (testing "log2"
    (is (= 0 (log2 1)))
    (is (= 1 (log2 2)))
    (is (= -1 (log2 1/2)))
    (is (= (/ 1 (log 2)) (log2 Math/E)))))

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

(deftest info-from-distribution-test
  (testing "info-from-distribution"
    (is (= 0.0 (info-from-distribution [1])))
    (is (= 1.0 (info-from-distribution [1/2 1/2])))
    (is (= 1.5 (info-from-distribution [1/4 1/4 1/2])))
    (is (= 1.75 (info-from-distribution [1/8 1/8 1/4 1/2])))))

(deftest info-from-probabilities-test
  (testing "info-from-probabilities"
    (is (= 0.0 (info-from-probabilities [1])))
    (is (= 1.0 (info-from-probabilities [1/2 1/2])))
    (is (= (double 5/3) (info-from-probabilities [1/4 1/4 1/2])))
    (is (= 2.25 (info-from-probabilities [1/8 1/8 1/4 1/2])))))

(deftest info-from-distribution-scaled-test
  (testing "info-from-distribution-scaled"
    (is (= 1.0 (info-from-distribution-scaled [1] [0.5])))
    (is (= 1.5 (info-from-distribution-scaled [1/2 1/2] [1 0.5])))
    (is (= 2.75 (info-from-distribution-scaled [1/4 1/4 1/2] [1 0.5 0.25])))
    (is
      (=
        2.75
        (info-from-distribution-scaled [1/8 1/8 1/4 1/2] [1 1 1 0.25])))))

(deftest info-test
  (testing "info"
    (is (= 0.0 (info [0 0 0 0])))
    (is (= 1.0 (info [0 1 0 1])))
    (is (= 1.5 (info [0 1 2 2])))
    (is (= 1.75 (info [0 0 1 0 0 1 2 3])))))

(deftest gaussian-test
  (testing "gaussian"
    (is (= 1 (gaussian 1 0)))
    (is (= (exp -2) (gaussian 1 2)))
    (is (= (exp -1/2) (gaussian 2 2)))))

(deftest gaussian-info-test
  (testing "gaussian-info"
    (is (= 0.0 (gaussian-info 1 [0 0 0 0])))
    (let [p [1/2 1/2]
          q [1 (exp -1/2)]]
      (is
        (=
          (info-from-distribution-scaled p q)
          (gaussian-info 1 [0 1 0 1]))))
    (let [p [1/4 1/4 1/2]
          q [1 (exp -1/2) (exp -2)]]
      (is
        (=
          (info-from-distribution-scaled p q)
          (gaussian-info 1 [0 1 2 2]))))))

;; TODO: write test for distribution-info
