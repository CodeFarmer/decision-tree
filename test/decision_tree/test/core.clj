(ns decision-tree.test.core
  (:use [decision-tree.core])
  (:use [clojure.test])
  (:require clojure.contrib.math))

(defn within? [delta x y]
  (< (clojure.contrib.math/abs (- x y)) delta))

(deftest test-log2

  (is (= 0.0 (log2 1)))
  (is (= 1.0 (log2 2)))
  (is (= 2.0 (log2 4))))

;; stealing the rspec tests from decision_tree.rb

(deftest test-entropy

  (is (= 0 (entropy [])) "entropy should be zero for an empty data set")
  
  (is (within? 0.0005 0.940 (entropy [ 
                                     [1,  "Y"]
                                     [2,  "Y"]
                                     [3,  "Y"]
                                     [4,  "Y"]
                                     [5,  "Y"]
                                     [6,  "Y"]
                                     [7,  "Y"]
                                     [8,  "Y"]
                                     [9,  "Y"]
                                     [10, "N"]
                                     [11, "N"]
                                     [12, "N"]
                                     [13, "N"]
                                     [14, "N"]]))
      "should match the value from the Mitchell book"))
