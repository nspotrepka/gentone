(ns gentone.time_test
  (:require [clojure.test      :refer :all]
            [gentone.time      :refer :all]
            [gentone.sequencer :refer :all]))

(def tt0 [0 1/4 1/2 3/4])
(def tt1 [0 1/2 3/4 7/8])
(def tt2 [1/8 3/8 1/2 5/6])
(def pp [0 0 0 0])
(def no-rests [false false false false])
(def some-rests [true false false true])
(def all-rests [true true true true])

(def full-seq (make-sequence tt0 pp no-rests))
(def some-seq (make-sequence tt1 pp some-rests))
(def none-seq (make-sequence tt2 pp all-rests))

(deftest times-test
  (testing "times"
    (is (= tt0 (times full-seq)))
    (is (= tt1 (times some-seq)))
    (is (= tt2 (times none-seq)))))

(deftest times-r-test
  (testing "times-r"
    (is (= tt0 (times-r full-seq)))
    (is (= [1/2 3/4 7/8] (times-r some-seq)))
    (is (= [] (times-r none-seq)))))

(deftest time-duration-test
  (testing "time-duration"
    (is (= [1] (time-duration [0])))
    (is (= [1/2 1/2] (time-duration [1/4 3/4])))
    (is (= [1/4 1/4 1/4 1/4] (time-duration tt0)))
    (is (= [1/2 1/4 1/8 1/8] (time-duration tt1)))
    (is (= [1/4 1/8 1/3 7/24] (time-duration tt2)))))
