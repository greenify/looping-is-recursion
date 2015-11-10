(ns looping-is-recursion)

(defn power [base exp]
   (let [helper (fn [acc n]
                 (if (zero? n)
                   acc
                   (recur (* acc base) (dec n))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (let [helper (fn [acc a-seq]
                 (if (empty? a-seq)
                   acc
                   (recur (first a-seq) (rest a-seq))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [acc iseq1 iseq2]
                 (if (or (empty? iseq1) (empty? iseq2))
                   (and acc (empty? iseq1) (empty? iseq2))
                   (recur (=(first iseq1) (first iseq2)) (rest iseq1) (rest iseq2))))]
  (helper true seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [index 0
         [f & fs] a-seq]
    (cond
     (nil? f) nil
     (pred f) index
     :else (recur (inc index) fs))))

(defn avg [a-seq]
  (loop [sum 0
         els 0
         [f & fs] a-seq]
    (if (nil? f) (/ sum els)
      (recur (+ sum f) (inc els) fs))))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem)  (conj a-set elem)))

(defn parity [a-seq]
  (loop [toggled #{}
         [f & fs] a-seq]
    (if (nil? f) toggled
      (recur (toggle toggled f) fs)
    )))

(defn fast-fibo [n]
  (loop [fib_n 0
         fib_n_1 1
         ns n]
    (if (= ns 0) fib_n
     (recur (+ fib_n fib_n_1) fib_n (dec ns) )
    )))

(defn cut-at-repetition [a-seq]
  (loop [els #{}
         acc []
         [f & fs] a-seq]
    (cond
       (nil? f) (seq els)
       (contains? els f) acc
       :else (recur (conj els f) (conj acc f) fs)
    )))

