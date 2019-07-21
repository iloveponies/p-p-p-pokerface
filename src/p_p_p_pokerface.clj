(ns p-p-p-pokerface)

(defn rank [card]
  (let [replacements {\T 10, \J 11, \Q 12, \K 13, \A 14}
        [rank-char _] card]
    (if (Character/isDigit rank-char)
      (Integer/valueOf (str rank-char))
      (get replacements rank-char))))

(defn suit [card]
  (let [[_ suit-char] card]
    (str suit-char)))

(defn rank-counts [hand]
  (let [ranks (map rank hand)]
    (sort (vals (frequencies ranks)))))

(defn n-of-a-kind? [hand n]
  (let [max-count (apply max (rank-counts hand))]
    (= n max-count)))

(defn pair? [hand]
  (n-of-a-kind? hand 2))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? hand 3))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? hand 4))

(defn flush? [hand]
  (let [suits (map suit hand)
        counts (vals (frequencies suits))
        min-count (apply min counts)]
    (= 5 min-count)))

(defn full-house? [hand]
  (let [full-house-counts (seq [2 3])]
    (= full-house-counts (rank-counts hand))))

(defn two-pairs? [hand]
  (let [two-pairs-counts (seq [1 2 2])]
    (or (four-of-a-kind? hand)
        (= two-pairs-counts (rank-counts hand)))))

(defn straight? [hand]
  (let [high-ace-ranks (sort (map rank hand))
        low-ace-ranks (sort (replace {14 1} high-ace-ranks))
        straight-from-min (fn [min-rank] (range min-rank (+ min-rank 5)))]
    (or (= high-ace-ranks (straight-from-min (apply min high-ace-ranks)))
        (= low-ace-ranks (straight-from-min (apply min low-ace-ranks))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1] [two-pairs? 2]
                   [three-of-a-kind? 3] [straight? 4] [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7] [straight-flush? 8]}
        match? (fn [pair] ((first pair) hand))
        values (fn [matches] (map second matches))]
    (apply max (values (filter match? checkers)))))
