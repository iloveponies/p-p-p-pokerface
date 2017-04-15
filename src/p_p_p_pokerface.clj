(ns p-p-p-pokerface)

(defn rank [card]
  (let [bigranks {\T 10, \J 11, \Q 12, \K 13, \A 14}
       [cards-rank _] card]
    (if (not (Character/isDigit cards-rank))
      (get bigranks cards-rank)
      (Integer/valueOf (str cards-rank)))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (== 2 (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (== 3 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (== 4 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (== 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (and (contains? (set (vals (frequencies (map rank hand)))) 3)
       (contains? (set (vals (frequencies (map rank hand)))) 2)))

(defn two-pairs? [hand]
  (let [filteredfreqs
        (filter
         (fn [freq] (or (== 2 freq) (== 4 freq)))
         (vals (frequencies (map rank hand))))]
    (or (= [2 2] filteredfreqs) (= [4] filteredfreqs))))

(defn straight? [hand]
  (let [sorted-hand-ranks (sort (map rank hand))
        min-rank (apply min sorted-hand-ranks)
        max-rank (apply max sorted-hand-ranks)]
    (if (and (== min-rank 2) (== max-rank 14))
        (let [new-sorted (sort (replace {14 1} sorted-hand-ranks))
              new-min (apply min new-sorted)
              new-max (apply max new-sorted)]
          (= new-sorted (range new-min (+ new-max 1))))
      (= sorted-hand-ranks (range min-rank (+ max-rank 1))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn hand-has-type? [hand checker-value]
  ((first checker-value) hand))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
    (apply max (map (fn [check-value]
                (let [[_ check] check-value]
                  (if (hand-has-type? hand check-value)
                    check
                    0))) checkers))))
