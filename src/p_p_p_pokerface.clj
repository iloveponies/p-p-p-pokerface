(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card
        rmap {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit r) (Integer/valueOf (str r)) (rmap r))))

(defn suit [card]
  (let [[_ suit] card] (str suit)))

(defn high-card? [hand] true)

(defn pair? [hand]
  (<= 2
      (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (<= 3
      (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (<= 4
      (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (= 5
     (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (= [2 3]
     (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (or (full-house? hand)
      (four-of-a-kind? hand)
      (= [1 2 2]
         (sort (vals (frequencies (map rank hand)))))))

(defn straight? [hand]
  (let [rseq (sort (map rank hand))
        rmin (first rseq)
        rmax (last rseq)
        equal? (fn [coll min] (= (range min (+ 5 min)) coll))]
    (or (equal? rseq rmin)
        (and (= 14 rmax) (equal? (sort (replace {14 1} rseq)) 1)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1]
                   [two-pairs? 2] [three-of-a-kind? 3]
                   [straight?  4] [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max
           (map second
                (filter (fn [x] ((first x) hand)) checkers)))))