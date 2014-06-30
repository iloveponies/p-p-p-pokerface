(ns p-p-p-pokerface)

(defn rank [card]
  (let [rank-map {\T 10, \J 11, \Q 12, \K 13, \A 14}
        rank-char (first card)]
    (if (Character/isDigit rank-char)
      (Integer/valueOf (str rank-char))
      (get rank-map rank-char))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn rank-freqs [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    freqs))

(defn rank-freqs-set [hand]
  (set (rank-freqs hand)))

(defn n-of-a-kind [hand n]
  (contains? (rank-freqs-set hand) n))

(defn high-card? [hand]
  true)

(defn pair? [hand]
  (n-of-a-kind hand 2))

(defn three-of-a-kind? [hand]
  (n-of-a-kind hand 3))

(defn four-of-a-kind? [hand]
  (n-of-a-kind hand 4))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (apply = suits)))

(defn full-house? [hand]
  (= (sort (rank-freqs-set hand)) [2 3]))

(defn two-pairs? [hand]
  (let [sorted-rank-freqs (sort (rank-freqs hand))]
    (or (= sorted-rank-freqs [1 2 2])
        (= sorted-rank-freqs [1 4]))))

(defn straight? [hand]
  (let [sorted-ranks (sort (map rank hand))
        min-sorted-ranks (apply min sorted-ranks)
        ace-as-rank-1 (sort (replace {14 1} sorted-ranks))
        min-ace-as-rank-1 (apply min ace-as-rank-1)]
    (or (= sorted-ranks (range min-sorted-ranks (+ min-sorted-ranks 5)))
        (= ace-as-rank-1 (range min-ace-as-rank-1 (+ min-ace-as-rank-1 5))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1]
                  [two-pairs? 2] [three-of-a-kind? 3]
                  [straight? 4] [flush? 5]
                  [full-house? 6] [four-of-a-kind? 7]
                  [straight-flush? 8]}
        matched-hands (filter (fn [[fst _]] (fst hand)) checkers)]
    (apply max (map second matched-hands))))

