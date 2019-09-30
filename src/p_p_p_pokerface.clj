(ns p-p-p-pokerface)

(def rank-values {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[rnk _] card]
    (if (Character/isDigit rnk)
      (Integer/valueOf (str rnk))
      (rank-values rnk))))

(defn suit [card]
  (let [[_ st] card]
    (str st)))

(defn pair? [hand]
  (let [ranks (map rank hand)
        rank-freqs (vals (frequencies ranks))
        pairs (frequencies rank-freqs)]
    (= (pairs 2) 1)))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
        rank-freqs (vals (frequencies ranks))
        threes (frequencies rank-freqs)]
    (= (threes 3) 1)))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)
        rank-freqs (vals (frequencies ranks))
        fours (frequencies rank-freqs)]
    (= (fours 4) 1)))

(defn flush? [hand]
  (let [suits (map suit hand)
        suits-freqs (vals (frequencies suits))]
    (= suits-freqs [5])))

(defn full-house? [hand]
  (let [ranks (map rank hand)
        rank-freqs (sort (vals (frequencies ranks)))]
    (= rank-freqs [2 3])))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        rank-freqs (sort (vals (frequencies ranks)))]
    (= rank-freqs [1 2 2])))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        min-rank (apply min ranks)]
    (or (= ranks (range min-rank (+ min-rank 5)))
        (= ranks [2 3 4 5 14]))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        checker-fn (fn [x]
                     (if ((first x) hand)
                       (second x)
                       -1))]
    (apply max (map checker-fn checkers))))
