(ns gentone.util_test
  (:require [clojure.test      :refer :all]
            [gentone.util      :refer :all]
            [gentone.sequencer :refer :all]))

(def tt [0 1/4 1/2 3/4])
(def pp [0 0 0 0])
(def rr0 [false false true true])
(def rr1 [false true false true])
(def rr2 [true false false true])
(def rr3 [true true false false])

(def seq0 (make-sequence tt pp rr0))
(def seq1 (make-sequence tt pp rr1))
(def seq2 (make-sequence tt pp rr2))
(def seq3 (make-sequence tt pp rr3))

(deftest rotate-left-test
  (testing "rotate-left"
    (is (= [] (rotate-left [])))
    (is (= [2 3 4 1] (rotate-left [1 2 3 4])))
    (is (= [8 7 6 9] (rotate-left [9 8 7 6])))
    (is (= [2 3 4 5 0 1] (rotate-left 2 [0 1 2 3 4 5])))
    (is (= [5 0 1 2 3 4] (rotate-left 5 [0 1 2 3 4 5])))))

(deftest rotate-right-test
  (testing "rotate-right"
    (is (= [] (rotate-right [])))
    (is (= [4 1 2 3] (rotate-right [1 2 3 4])))
    (is (= [6 9 8 7] (rotate-right [9 8 7 6])))
    (is (= [4 5 0 1 2 3] (rotate-right 2 [0 1 2 3 4 5])))
    (is (= [1 2 3 4 5 0] (rotate-right 5 [0 1 2 3 4 5])))))

(deftest echo-rests-test
  (testing "echo-rests"
    (is (= [false false false true] (echo-rests rr0)))
    (is (= [false false false false] (echo-rests rr1)))
    (is (= [true false false false] (echo-rests rr2)))
    (is (= [false true false false] (echo-rests rr3)))))

(deftest filter-rests-test
  (testing "filter-rests"
    (is (= [0 1] (filter-rests [0 1 2 3] rr0)))
    (is (= [0 2] (filter-rests [0 1 2 3] rr1)))
    (is (= [1 2] (filter-rests [0 1 2 3] rr2)))
    (is (= [2 3] (filter-rests [0 1 2 3] rr3)))))

(deftest filter-rests-echo-test
  (testing "filter-rests-echo"
    (is (= [0 1 2] (filter-rests-echo [0 1 2 3] rr0)))
    (is (= [0 1 2 3] (filter-rests-echo [0 1 2 3] rr1)))
    (is (= [1 2 3] (filter-rests-echo [0 1 2 3] rr2)))
    (is (= [0 2 3] (filter-rests-echo [0 1 2 3] rr3)))))

(deftest switch-rests-test
  (testing "switch-rests"
    (is (= [0 1 8 9] (switch-rests [0 1 2 3] [6 7 8 9] rr0)))
    (is (= [0 7 2 9] (switch-rests [0 1 2 3] [6 7 8 9] rr1)))
    (is (= [6 1 2 9] (switch-rests [0 1 2 3] [6 7 8 9] rr2)))
    (is (= [6 7 2 3] (switch-rests [0 1 2 3] [6 7 8 9] rr3)))))

(deftest rests-r-test
  (testing "rests-r"
    (is (= [false false true] (rests-r seq0)))
    (is (= [false true false true] (rests-r seq1)))
    (is (= [false false true] (rests-r seq2)))
    (is (= [true false false] (rests-r seq3)))))
