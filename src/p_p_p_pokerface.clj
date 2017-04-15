(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card
        ranks {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit rank) (Integer/valueOf (str rank))
      (get ranks rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (== (count (frequencies (map suit hand))) 1))

(defn full-house? [hand]
  (if (and (not (four-of-a-kind? hand))
           (and (three-of-a-kind? hand) (pair? hand)))
    (== (count (frequencies (map rank hand))) 2) false))

(defn two-pairs? [hand]
  (let [counts (vals (frequencies (map rank hand)))
        freqs (filter (fn [freq] (>= freq 2)) counts)]
    (cond (four-of-a-kind? hand) true
          (== (count freqs) 2) true
          :else false)))

(defn straight? [hand]
  (let [sorted-hand-ranks (sort (map rank hand))
        min-rank (apply min sorted-hand-ranks)
        max-rank (apply max sorted-hand-ranks)
        rank-range (range min-rank (+ max-rank 1))
        ace-involved (=(apply max sorted-hand-ranks) 14)]
    (if (and (== (count rank-range) 5)
             (= rank-range sorted-hand-ranks)) true
      (if (boolean ace-involved)
        (if (= sorted-hand-ranks [2 3 4 5 14]) true false) false))))

(defn straight-flush? [hand]
  (if (and (straight? hand) (flush? hand)) true false))

(defn high-card? [hand]
  true)

(defn value [hand]
  (cond (straight-flush? hand)  8
        (four-of-a-kind? hand)  7
        (full-house? hand)      6
        (flush? hand)           5
        (straight? hand)        4
        (three-of-a-kind? hand) 3
        (two-pairs? hand)       2
        (pair? hand)            1
        (high-card? hand)       0))
