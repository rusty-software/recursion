(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (not (seq coll))
    false
    (not (seq (rest coll)))))

(defn my-last [coll]
  (if (not (seq (rest coll)))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (max (first a-seq) (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq)
    false

    (not= elem (first a-seq))
    (sequence-contains? elem (rest a-seq))

    :else
    true))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq)
    a-seq

    (pred? (first a-seq))
    (cons (first a-seq) (my-take-while pred? (rest a-seq)))

    :else
    '()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq)
    a-seq

    (pred? (first a-seq))
    (my-drop-while pred? (rest a-seq))

    :else
    a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (not= (count a-seq) (count b-seq)) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (if (and (seq seq-1) (seq seq-2))
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))
    '()))

(defn power [n k]
  (if (= k 0)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (> 1 n)
    0

    (= 1 n)
    1

    :else
    (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (> 1 how-many-times)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (> 1 up-to)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (> 1 (count a-seq))
    (vector a-seq)
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (> 1 (count a-seq))
    (vector '())
    (cons a-seq (inits (butlast a-seq)))))

(defn count-rotations [i a-seq]
  (if (= i (count a-seq))
    '()
    (cons a-seq (count-rotations (inc i) (concat (rest a-seq) (vector (first a-seq)))))))

(defn rotations [a-seq]
  (if (not (seq a-seq))
    (vector '())
    (count-rotations 0 a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [elem (first a-seq)
          old-count (get freqs elem)
          new-freqs (if old-count
                       (assoc freqs elem (inc old-count))
                       (assoc freqs elem 1))]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies-helper [coll a-map]
  (if (empty? a-map)
    coll
    (let [[k v] (first a-map)
          new-coll (concat coll (repeat v k))]
      (un-frequencies-helper new-coll (rest a-map)))))

(defn un-frequencies [a-map]
  (un-frequencies-helper '() a-map))

(defn my-take [n coll]
  (if (or (zero? n)
          (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (zero? n)
          (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half-count (int (/ (count a-seq) 2))]
    [(my-take half-count a-seq) (my-drop half-count a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    :else (if (< (first a-seq) (first b-seq))
            (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
            (cons (first b-seq) (seq-merge (rest b-seq) a-seq)))))

(defn merge-sort [a-seq]
  (if (> 2 (count a-seq))
    a-seq
    (let [[half1 half2] (halve a-seq)]
      (seq-merge (merge-sort half1) (merge-sort half2)))))

(defn split-into-monotonics [a-seq]
  [:-])

#_(if (not (seq a-seq))
    '(())
    (vec (set (concat (map (fn [items] (concat [ (first a-seq)] items)) (rotations (rest a-seq)))
                 (map (fn [items] (concat items [ (first a-seq)])) (rotations (rest a-seq)))))))

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

