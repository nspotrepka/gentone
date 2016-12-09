(ns gentone.pitch_test
  (:require [clojure.test      :refer :all]
            [gentone.sequencer :refer :all]
            [gentone.pitch     :refer :all]))

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

(deftest rests-test
  (testing "rests"
    (is (= no-rests (rests full-seq)))
    (is (= some-rests (rests some-seq)))
    (is (= all-rests (rests none-seq)))))

(deftest pitches-r-test
  (testing "pitches-r"
    (is (= pp (pitches-r full-seq)))
    (is (= [7 2 7 7] (pitches-r some-seq)))
    (is (= [nil nil nil nil] (pitches-r none-seq)))))
