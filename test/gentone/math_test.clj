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

(deftest entropy-from-distribution-test
  (testing "entropy-from-distribution"
    (is (= 0.0 (entropy-from-distribution [1])))
    (is (= 1.0 (entropy-from-distribution [1/2 1/2])))
    (is (= 1.5 (entropy-from-distribution [1/4 1/4 1/2])))
    (is (= 1.75 (entropy-from-distribution [1/8 1/8 1/4 1/2])))))

(deftest entropy-from-probabilities-test
  (testing "entropy-from-probabilities"
    (is (= 0.0 (entropy-from-probabilities [1])))
    (is (= 1.0 (entropy-from-probabilities [1/2 1/2])))
    (is (= (double 5/3) (entropy-from-probabilities [1/4 1/4 1/2])))
    (is (= (double 9/4) (entropy-from-probabilities [1/8 1/8 1/4 1/2])))))

(deftest entropy-test
  (testing "entropy"
    (is (= 0.0 (entropy [0 0 0 0])))
    (is (= 1.0 (entropy [0 1 0 1])))
    (is (= 1.5 (entropy [0 1 2 2])))
    (is (= 1.75 (entropy [0 0 1 0 0 1 2 3])))))

(deftest gaussian-test
  (testing "gaussian"
    (is (= 1 (gaussian 1 0)))
    (is (= (exp -2) (gaussian 1 2)))
    (is (= (exp -1/2) (gaussian 2 2)))))

(deftest gaussian-entropy-test
  (testing "gaussian-entropy"
    (is (= 0.0 (gaussian-entropy 1 [0 0 0 0])))
    (let [a [1 (exp -1/2)]
          p (normalize a)]
      (is
        (=
          (entropy-from-distribution p)
          (gaussian-entropy 1 [0 1 0 1]))))
    (let [a [1 (exp -1/2) (* 2 (exp -2))]
          p (normalize a)]
      (is
        (=
          (entropy-from-distribution p)
          (gaussian-entropy 1 [0 1 2 2]))))))

;; TODO: write test for distribution-entropy
