(ns looping-is-recursion)

(defn power [base exp] (let [c (- exp 1)
                             acc base
                             helper (fn [acc base exp c]
                             (cond
                              (zero? base)
                                0
                              (zero? exp)
                                1
                              (zero? c)
                                acc
                              :else
                                (recur (* acc base) base exp (dec c))))]
                         (helper acc base exp c)))
;  ":(")

(defn last-element [a-seq] (let [helper (fn [a-seq lastelem]
                                 (if (nil? (first a-seq))
                                   lastelem
                                   (recur (rest a-seq) (first a-seq))))]
                             (helper a-seq nil)))

;  ":(")

(defn seq= [seq1 seq2] (let [helper (fn [seq1 seq2 eq]
                                      (cond
                                        (and (nil? (first seq1)) (nil? (first seq2)))
                                          (if (nil? eq)
                                            true                                 ; both strings are nil so they match
                                            eq)                                  ; we are done matching
                                        (or (nil? (first seq1)) (nil? (first seq2)))
                                            false                                ; one string ended before the other
                                        (and (= (first seq1) (first seq2)))
                                            (recur (rest seq1) (rest seq2) true) ; check the next position
                                        :else
                                            false))]                             ; the strings don't match, exit here
                         (helper seq1 seq2 nil)))

;  ":(")

(defn find-first-index [pred a-seq] (loop [pseq a-seq
                                           idx 0]
                                      (let [cur (first pseq)]
                                        (if (nil? cur)
                                          nil
                                          (if (pred cur)
                                            idx
                                            (recur (rest pseq) (inc idx)))))))
;  ":(")

(defn avg [a-seq] (loop [tot 0.0
                         tnum 0
                         pseq a-seq]
                    (if (nil? a-seq)
                      nil
                      (if (or (nil? pseq) (nil? (first pseq)))
                        (/ tot tnum)
                        (recur (+ tot (first pseq)) (inc tnum) (rest pseq))))))
;  -1)

(defn parity [a-seq] (loop [items {}
                            pseq a-seq]
                       (if (nil? a-seq)
                         nil
                         (if (or (nil? pseq) (nil? (first pseq)))
                           (set (keys (filter #(odd? (second %)) items)))
                           (let [cur (first pseq)]          ; current item from sequence
                             (recur (if (nil? (get items cur)) ; does it already exist in the items set?
                                      (assoc items cur 1)      ; if not then create a new item
                                      (assoc items cur (inc (get items cur)))) (rest pseq)) ; exists so increment count
                               )))))

;  ":(")

(defn fast-fibo [n]
  (if (< n 2)
    n
    (loop [ x [0 1]]
      (if (>= (count x) n)
        (apply + (take-last 2 (subvec x 0 n)))
        (let [[n1 n2] (reverse x)]
          (recur (conj x (+ n1 n2))))))))

;(defn fast-fibo [n]
;  (if(< n 2)
;    n
;    (loop [ x [0 1]]
;      (if (< (count x) n)
;        (recur (conj x (+ (last x) (nth x (- (count x) 2)))))
;        (apply + (take-last 2 x))))))



;  ":(")

(defn cut-at-repetition [a-seq]
  [":("])

