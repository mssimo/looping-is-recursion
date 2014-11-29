(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [a-base acc n]
                 (if (zero? n)
                   acc
                   (recur a-base (* acc a-base) (dec n))))]
    (if (zero? exp)
      1
      (helper base base (dec exp)))))

(defn last-element [a-seq]
  (let [helper (fn [a-seqe]
                 (if (= (count a-seqe) 1)
                   (first a-seqe)
                   (recur (rest a-seqe))))]
    (if (empty? a-seq)
      nil
      (helper a-seq))))

(defn seq= [seq1 seq2]
  (let [helper (fn [a-seqe1 a-seqe2]
                 (if (and (empty? a-seqe1) (empty? a-seqe2))
                   true
                   (if (and (first a-seqe1) (first a-seqe2))
                     (if (= (first a-seqe1) (first a-seqe2))
                       (recur (rest a-seqe1) (rest a-seqe2))
                       false)
                     false)))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [index 0
         a-seqe a-seq]
    (if (empty? a-seqe)
      nil
      (if (pred (first a-seqe))
        index
        (recur (inc index) (rest a-seqe))))))

(defn avg [a-seq]
  (loop [sum 0
         a-seq-count 0
         a-seqe a-seq]
    (if (and (= a-seq-count 0) (empty? a-seqe))
      nil
      (if (empty? a-seqe)
        (/ sum a-seq-count)
        (recur (+ sum (first a-seqe)) (inc a-seq-count) (rest a-seqe))))))

(defn equal? [n]
  (fn [k] (= k n)))

(defn not-equal? [n]
  (fn [k] (not= k n)))

(defn parity [a-seq]
  (loop [a-seqe a-seq
         a-set #{}]
    (if (empty? a-seqe)
      a-set
      ; count the times that the first element occurs in the sequence
      ; if odd times add to set
      (if (= (mod (count (filter (equal? (first a-seqe)) a-seqe)) 2) 1)
        (recur (filter (not-equal? (first a-seqe)) a-seqe) (conj a-set (first a-seqe)))
        (recur (filter (not-equal? (first a-seqe)) a-seqe) a-set)
        ))))

(defn fast-fibo [n]
  (loop [curr 1
         previous 0
         counter 1]
    (if (or (= n 0) (= n 1))
      n
      (if (= counter n)
        curr
        (recur (+ curr previous) curr (inc counter))))))

(defn cut-at-repetition [a-seq]
  (let [first-index (find-first-index (equal? (first a-seq)) (rest a-seq))]
    (if (not first-index)
      a-seq
      (loop [a-seqe a-seq
             elements []
             at-index 0]
        (if (= first-index at-index)
          (conj elements (first a-seqe))
          (recur (rest a-seqe) (conj elements (first a-seqe)) (inc at-index)))))))

