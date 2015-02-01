(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _]    card
        rank-map {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (or (get rank-map r)
        (Integer/valueOf (str r)))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn rank-freqs [hand]
  (frequencies (map rank hand)))

(defn rank-counts [hand]
  (vals (rank-freqs hand)))

(defn pair? [hand]
  (== (apply max (rank-counts hand))
      2))

(defn three-of-a-kind? [hand]
  (== (apply max (rank-counts hand))
      3))

(defn four-of-a-kind? [hand]
  (== (apply max (rank-counts hand))
      4))

(defn flush? [hand]
  (== (count (frequencies (map suit hand)))
      1))

(defn full-house? [hand]
  (= '(2 3)
     (sort (rank-counts hand))))

(defn two-pairs? [hand]
  (= '(1 2 2)
     (sort (rank-counts hand))))

(defn straight? [hand]
  (let [ranks    (sort (map rank hand))
        min-rank (apply min ranks)]
    (or (= (range min-rank (+ 5 min-rank))
           ranks)
        (= '(2 3 4 5 14)
           ranks))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn value [hand]
  (let [checkers          #{[high-card? 0]  [pair? 1]
                            [two-pairs? 2]  [three-of-a-kind? 3]
                            [straight? 4]   [flush? 5]
                            [full-house? 6] [four-of-a-kind? 7]
                            [straight-flush? 8]}
        checker->bool     (fn [chkr] [((first chkr) hand) (second chkr)])
        mapped-checkers   (map checker->bool checkers)
        filtered-checkers (filter first mapped-checkers)
        filtered-values   (map second filtered-checkers)]
    (apply max filtered-values)))
