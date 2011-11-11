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


(deftest test-partition-by

  (is (= {} (partition-by identity [])) "should return an empty map when the input seq is empty")

  (is (= {:a [:a], :b [:b], :c [:c]} (partition-by identity [:a :b :c])) "should return a map of x to a list containing each of x, for each member of a seq with no repeats when passed the identity function")

  (is (= {:a [:a :a], :b [:b]} (partition-by identity [:a :a :b])) "should return a map of x followed by the repeated elements, for a seq with repeats and the identity function")

  (is (= {1 [{:a 1 :b 2}], 2 [{:a 2 :b 1} {:a 2 :b 2}]} (partition-by :a [{:a 1 :b 2} {:a 2 :b 1} {:a 2 :b 2}])) "Should be able to partition a list of maps, using a keyword as the function argument (since this is the actual use case for decision-tree)"))


(deftest test-all-keys

  (is (= #{:a :b :c :d} (all-keys [{:a 1 :b 2} {:c 1 :d 2}])) "should deal with the simple case of completely disjoint maps")

  (is (= #{:a :b :c} (all-keys [{:a 1 :b 2} {:c 1 :a 2}])) "should return each key once, when there are repeated keys"))


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


(def sample-data

  [[{:outlook "Sunny",    :temperature "Hot",   :humidity  "High",   :wind  "Weak"},    "No"],
   [{:outlook "Sunny",    :temperature "Hot",   :humidity  "High",   :wind  "Strong"},  "No"],
   [{:outlook "Overcast", :temperature  "Hot",  :humidity  "High",   :wind  "Weak"},   "Yes"],
   [{:outlook "Rain",     :temperature  "Mild", :humidity  "High",   :wind  "Weak"},   "Yes"],
   [{:outlook "Rain",     :temperature  "Cool", :humidity  "Normal", :wind  "Weak"},   "Yes"],
   [{:outlook "Rain",     :temperature  "Cool", :humidity  "Normal", :wind  "Strong"}, "No"],
   [{:outlook "Overcast", :temperature  "Cool", :humidity  "Normal", :wind  "Strong"}, "Yes"],
   [{:outlook "Sunny",    :temperature  "Mild", :humidity  "High",   :wind  "Weak"},   "No"],
   [{:outlook "Sunny",    :temperature  "Cool", :humidity  "Normal", :wind  "Weak"},   "Yes"],
   [{:outlook "Rain",     :temperature  "Mild", :humidity  "Normal", :wind  "Weak"},   "Yes"],
   [{:outlook "Sunny",    :temperature  "Mild", :humidity  "Normal", :wind  "Strong"}, "Yes"],
   [{:outlook "Overcast", :temperature  "Mild", :humidity  "High",   :wind  "Strong"}, "Yes"],
   [{:outlook "Overcast", :temperature  "Hot",  :humidity  "Normal", :wind  "Weak"},   "Yes"],
   [{:outlook "Rain",     :temperature  "Mild", :humidity  "High",   :wind  "Strong"}, "No"]]
)


(deftest test-gain
  
  (is (within? 0.0005 0.247 (gain :outlook sample-data)) "should return 0.247 for :outlook given the sample data")

  (is (within? 0.0005 0.152 (gain :humidity sample-data)) "should return 0.152 for :humidity given the sample data")

  (is (within? 0.0005 0.048 (gain :wind sample-data)) "should return 0.048 for :wind")

  (is (within? 0.0005 0.029 (gain :temperature sample-data)) "should return 0.029 for :temperature"))


(deftest test-most-informative-key

  (is (= :outlook (most-informative-key sample-data)) "should return :outlook as the first choice given the sample data"))


(deftest test-build-decision-tree

  (is (nil? (build-decision-tree [])) "should return nil when the input is empty")

  (is (= :c (build-decision-tree [[{:a 1 :b 2} :c]])) "Should return the only output as a leaf node when there is only one input")

  (is (= :c (build-decision-tree [[{:a 1 :b 2} :c] [{:a 3 :b 4} :c]])) "Should return the only output when all the outputs are the same")

  (is (=
       [:outlook { "Sunny" [:humidity { "High"   "No"
                                        "Normal" "Yes"}]
                   "Overcast"  "Yes"
                   "Rain"  [:wind     { "Weak"   "Yes"
                                        "Strong" "No"}]}]
       (build-decision-tree sample-data)) "should build the correct decision tree given the sample data from the textbook"))


(deftest test-map-vals
  (is (= {:a 2 :b 3} (map-vals {:a 1 :b 2} inc))))


(deftest test-tree-decide

  (is (= "No" (tree-decide (build-decision-tree sample-data) {:outlook "Sunny" :humidity "High"})) "Should decide No Tennis when sunny and humid, given the sample data")

  (is (= "Yes" (tree-decide (build-decision-tree sample-data) {:outlook "Overcast"})) "Should decide Yes when the outlook is overcast"))
