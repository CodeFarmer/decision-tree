(ns decision-tree.core)

(defn log2 [x]
  (/ (Math/log x) (Math/log 2)))

(defn entropy [alist]

  "Given a list of pairs if (input, output), calculate the entropy of the list with respect to output"

  (let [n (count alist)
        counts (vals (frequencies (map #(nth % 1) alist)))]
    
    (apply + (map #(* (- %) (log2 %)) (map #(/ % n) counts)))))


(defn map-by
  
  ([afn aseq]
     
     "Given function and a seq, return a map of values the function can take when using members of the seq as an argument, to sub-seqs containing the parts of seq corresponding to those outputs"

     (map-by {} afn aseq))

  ([acc afn aseq]

     (if (empty? aseq)
       acc
       (let [i (first aseq) o (afn i) inputs (get acc o)]
         (if inputs
           (recur
            (assoc acc o (conj inputs i)) afn (rest aseq))
           (recur
            (assoc acc o [i]) afn (rest aseq)))))))