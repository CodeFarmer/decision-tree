(ns decision-tree.core)

(defn log2 [x]
  (/ (Math/log x) (Math/log 2)))

(defn entropy [alist]

  "Given a list of pairs if (input, output), calculate the entropy of the list with respect to output"

  (let [n (count alist) counts (vals (frequencies (map #(nth % 1) alist)))]
    (apply + (map #(* (- %) (log2 %)) (map #(/ % n) counts)))))
