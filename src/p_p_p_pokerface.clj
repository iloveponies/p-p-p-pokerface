(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card
        all-ranks {\T 10, \J 11, \Q 12, \K 13, \A 14}]
  (if (Character/isDigit rank)
    (Integer/valueOf (str rank))
    (all-ranks rank))))

(defn suit [card]
  (let [[_ suit] card]
  (str suit)))

(defn pair? [hand]
  (let [ranks (map rank hand)
        rank-freqs (vals (frequencies ranks))]
    (== (apply max rank-freqs) 2)))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
        rank-freqs (vals (frequencies ranks))]
    (and (== (apply max rank-freqs) 3) (not (pair? hand)))))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)
        rank-freqs (vals (frequencies ranks))]
    (== (apply max rank-freqs) 4)))

(defn flush? [hand]
  (let [suits (map suit hand)
        suit-freqs (vals (frequencies suits))]
    (== (apply max suit-freqs) 5)))

(defn full-house? [hand]
  (let [ranks (map rank hand)
        rank-freqs (vals (frequencies ranks))]
    (= (sort rank-freqs) [2 3])))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        rank-freqs (vals (frequencies ranks))]
    (or (= (sort rank-freqs) [1 2 2]) (four-of-a-kind? hand))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        ace-high (sort ranks)
        ace-low (sort (replace {14 1} ranks))
        sample-high (range (apply min ace-high)  (+ (apply max ace-high) 1))
        sample-low (range (apply min ace-low)  (+ (apply max ace-low) 1))]
    (or (= ace-high sample-high) (= ace-low sample-low))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [high-card? (fn [hand] true)
        checkers #{[high-card? 0]
                   [pair? 1]
                   [two-pairs? 2]
                   [three-of-a-kind? 3]
                   [straight? 4]
                   [flush? 5]
                   [full-house? 6]
                   [four-of-a-kind? 7]
                   [straight-flush? 8]}
        all-pairs (filter (fn [checker] ((checker 0) hand)) checkers)]
    (apply max (map second all-pairs))))
