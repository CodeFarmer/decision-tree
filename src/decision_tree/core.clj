(ns decision-tree.core)

(defn log2 [x]
  (/ (Math/log x) (Math/log 2)))

(defn entropy [alist]

  "Given a list of pairs if (input, output), calculate the entropy of the list with respect to output"

  (let [n (count alist)
        counts (vals (frequencies (map #(nth % 1) alist)))]
    
    (reduce + (map #(* (- %) (log2 %)) (map #(/ % n) counts)))))


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

(defn all-keys [aseq]

  "Given a list of maps, return a set containing all the keys from those maps"

  (into #{} (flatten (map keys aseq))))

(defn gain
  ([k alist]

     "Given a key and a list of pairs of (map, output), return the net reduction in entropy caused by partitioning the list according to the values associated with key in the input maps"
               
     (gain (entropy alist) k alist))

  ([current-entropy k alist]
     (let [parts (vals (map-by #(k (first %)) alist))]
       (- current-entropy
          (reduce +
           (map #(* (entropy %) (/ (count %) (count alist))) parts))))))

(defn most-informative-key [aseq]
  "Given a sequence of pairs of (map, output), return the map key that provides the highest information gain about outout, when used to partition the sequence"
  (apply max-key #(gain % aseq) (all-keys (map first aseq))))
