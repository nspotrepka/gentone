(ns gentone.pitch_test
  (:require [clojure.test      :refer :all]
            [gentone.pitch     :refer :all]
            [gentone.sequencer :refer :all]))

(def tt [0 1/4 1/2 3/4])
(def pp [0 2 7 5])
(def no-rests [false false false false])
(def some-rests [true false false true])
(def all-rests [true true true true])

(def full-seq (make-sequence tt pp no-rests))
(def some-seq (make-sequence tt pp some-rests))
(def none-seq (make-sequence tt pp all-rests))

(deftest pitches-test
  (testing "pitches"
    (is (= pp (pitches full-seq)))
    (is (= pp (pitches some-seq)))
    (is (= pp (pitches none-seq)))))

(deftest pitches-r-test
  (testing "pitches-r"
    (is (= pp (pitches-r full-seq)))
    (is (= [2 7 5] (pitches-r some-seq)))
    (is (= [] (pitches-r none-seq)))))

(deftest pitch-freq-test
  (testing "pitch-freq"
    (is (= [100 200 50 100] (pitch-freq 100 [0 12 -12 0])))
    (is (= [1 1 1 (Math/pow 2 1/12)] (pitch-freq 1 [0 0 0 1])))))
