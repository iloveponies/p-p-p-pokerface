(ns p-p-p-pokerface)

(defn rank [card]
  (let [replacements {\T 10, \J 11, \Q 12, \K 13, \A 14}
        [rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (replacements rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn same-rank-frequency? [number, hand]
  (if (<= number (apply max (vals (frequencies (map rank hand)))))
    true
    false))

(defn same-suit-frequency? [number, hand]
  (if (<= number (apply max (vals (frequencies (map suit hand)))))
    true
    false))

(defn pair? [hand]
  (same-rank-frequency? 2 hand))

(defn three-of-a-kind? [hand]
  (same-rank-frequency? 3 hand))

(defn four-of-a-kind? [hand]
  (same-rank-frequency? 4 hand))

(defn flush? [hand]
  (same-suit-frequency? 5 hand))

(defn full-house? [hand]
  (if (= (range 2 4) (sort (vals (frequencies (map rank hand)))))
    true
    false))

(defn two-pairs? [hand]
  (let [sorted-value-frequencies (sort (vals (frequencies (map rank hand))))]
  (cond
   (= (seq [1 2 2]) (seq sorted-value-frequencies)) true
   (= (seq [1 4]) (seq sorted-value-frequencies)) true
   :else false)))

(defn straight? [hand]
  (let [sorted-values (sort (seq (map rank hand)))
        straight-end (+ 1 (apply max sorted-values))
        straight-start (- straight-end 5)]
    (cond
     (= (seq [2 3 4 5 14]) sorted-values) true
     (= (range straight-start straight-end) sorted-values) true
     :else false)))

(defn straight-flush? [hand]
  (and (straight? hand)(flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1]
                   [two-pairs? 2] [three-of-a-kind? 3]
                   [straight? 4]  [flush? 5]
                   [full-house? 6][four-of-a-kind? 7]
                   [straight-flush? 8]}
        has-value (fn [checker] [((first checker) hand) (second checker)])]
    (apply max (map second (filter first (map has-value checkers))))))
