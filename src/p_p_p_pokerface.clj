(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card
        rank-values {\T 10,
                     \J 11,
                     \Q 12,
                     \K 13,
                     \A 14}]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (get rank-values rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn n-of-a-kind? [n, hand]
  (let [ranks (map rank hand)
        rank-freqs (frequencies ranks)]
    (>= (apply max (vals rank-freqs)) n)))

(defn pair? [hand]
  (n-of-a-kind? 2 hand))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? 3 hand))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? 4 hand))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (apply = suits)))

(defn full-house? [hand]
  (let [ranks (map rank hand)
        rank-freqs (frequencies ranks)
        rank-freqs-vals (vals rank-freqs)
        sorted-vals (sort rank-freqs-vals)]
    (= sorted-vals (range 2 4))))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        rank-freqs (frequencies ranks)
        rank-freqs-vals (vals rank-freqs)
        sorted-vals (sort rank-freqs-vals)]
    (or (four-of-a-kind? hand) (= sorted-vals [1 2 2]))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        ranks-low-ace (replace {14 1} ranks)
        ranks-sorted (sort ranks)
        ranks-low-sorted (sort ranks-low-ace)
        first-ranks (first ranks-sorted)
        first-ranks-low (first ranks-low-sorted)]
    (or (= ranks-sorted (range first-ranks (+ first-ranks 5)))
        (= ranks-low-sorted (range first-ranks-low (+ first-ranks-low 5))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (cond
    (straight-flush? hand)  8
    (four-of-a-kind? hand)  7
    (full-house? hand)      6
    (flush? hand)           5
    (straight? hand)        4
    (three-of-a-kind? hand) 3
    (two-pairs? hand)       2
    (pair? hand)            1
    :else                   0))
