(ns gentone.time_test
  (:require [clojure.test      :refer :all]
            [gentone.sequencer :refer :all]
            [gentone.time      :refer :all]))

(def tt0 [0 1/4 1/2 3/4])
(def tt1 [0 1/2 3/4 7/8])
(def tt2 [1/8 3/8 1/2 5/6])

(def seq0 (make-sequence tt0 (repeat 4 0)))
(def seq1 (make-sequence tt1 (repeat 4 0)))
(def seq2 (make-sequence tt2 (repeat 4 0)))

(deftest times-test
  (testing "times"
    (is (= tt0 (times seq0)))
    (is (= tt1 (times seq1)))
    (is (= tt2 (times seq2)))))

(deftest durations-test
  (testing "durations"
    (is (= [1/4 1/4 1/4 1/4] (durations seq0)))
    (is (= [1/2 1/4 1/8 1/8] (durations seq1)))
    (is (= [1/4 1/8 1/3 7/24] (durations seq2)))))
