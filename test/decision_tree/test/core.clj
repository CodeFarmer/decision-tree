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

(deftest test-map-by

  (is (= {} (map-by identity [])) "should return an empty map when the input seq is empty")

  (is (= {:a [:a], :b [:b], :c [:c]} (map-by identity [:a :b :c])) "should return a map of x to a list containing each of x, for each member of a seq with no repeats when passed the identity function")

  (is (= {:a [:a :a], :b [:b]} (map-by identity [:a :a :b])) "should return a map of x followed by the repeated elements, for a seq with repeats and the identity function")

  (is (= {1 [{:a 1 :b 2}], 2 [{:a 2 :b 1} {:a 2 :b 2}]} (map-by :a [{:a 1 :b 2} {:a 2 :b 1} {:a 2 :b 2}])) "Should be able to partition a list of maps, using a keyword as the function argument (since this is the actual use case for decision-tree)"))


;; stealing the rspec tests from decision_tree.rb

(deftest test-entropy

  (is (= 0 (entropy [])) "entropy should be zero for an empty data set")
  
  (is (within? 0.0005 0.940
               (entropy [ 
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

