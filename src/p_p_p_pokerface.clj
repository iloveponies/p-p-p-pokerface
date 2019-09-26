(ns p-p-p-pokerface)

(defn rank [card]
  (let [replacements {\T 10, \J 11, \Q 12, \K 13, \A 14}
        [fst _] card]
    (cond 
      (Character/isDigit fst) (Integer/valueOf (str fst))
      :else (get replacements fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn has-num-kind? [num hand]
  "from a sequence of ranks, calculate frequency value for each rank
   and check if any of the value is at most num value."
  (>= (apply max (vals (frequencies (map rank hand)))) num))

(defn pair? [hand]
  (has-num-kind? 2 hand))

(defn three-of-a-kind? [hand]
  (has-num-kind? 3 hand))

(defn four-of-a-kind? [hand]
  (has-num-kind? 4 hand))

(defn flush? [hand]
  (== (count (set (map suit hand))) 1))

(defn rank-range [hand]
  (sort (vals (frequencies (map rank hand)))))

(defn full-house? [hand]
  (= (rank-range hand) [2 3]))

(defn two-pairs? [hand]
  (let [ranks (rank-range hand)]
    (cond 
      (= ranks [1 2 2]) true  ; two-pairs
      (= ranks [1 4]) true    ; four-of-a-kind
      :else false)))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        replace-map {14 1}
        needs-replace? (and (== (first ranks) 2) (== (last ranks) 14))
        replaced-ranks (if needs-replace? (sort (replace replace-map ranks)) ranks)
        rank-min (first replaced-ranks)
        compare-range (range rank-min (+ rank-min 5))]

        (= replaced-ranks compare-range)))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0] 
                   [pair? 1]
                   [two-pairs? 2]
                   [three-of-a-kind? 3]
                   [straight? 4]
                   [flush? 5]
                   [full-house? 6]
                   [four-of-a-kind? 7]
                   [straight-flush? 8]}
        checked-hands (map (fn [[checker-fn hand-val]] [(checker-fn hand) hand-val]) checkers)
        valid-hands (filter (fn [checked-hand] (first checked-hand)) checked-hands)
        hand-vals (map second valid-hands)]

        (apply max hand-vals)))
