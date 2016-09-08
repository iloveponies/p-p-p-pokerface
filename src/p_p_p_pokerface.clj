(ns p-p-p-pokerface)

(defn rank [card]
  (let [rank-to-int {"2" 2
                     "3" 3
                     "4" 4
                     "5" 5
                     "6" 6
                     "7" 7
                     "8" 8
                     "9" 9
                     "T" 10
                     "J" 11
                     "Q" 12
                     "K" 13
                     "A" 14}
        [r _] card]
    (get rank-to-int (str r))))

(defn suit [card]
  (let [[r s] card]
    (str s)))

(defn n-of-a-kind? [hand n ]
  (let [ranks (map rank hand)
        ranks-counts (set (vals (frequencies ranks)))]
    (contains? ranks-counts n)))

(defn pair? [hand]
  (n-of-a-kind? hand 2))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? hand 3))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? hand 4))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (= 1 (count (set suits)))))

(defn full-house? [hand]
  (let [ranks (map rank hand)
        ranks-counts (set (vals (frequencies ranks)))]
    (= (range 2 4) (sort ranks-counts))))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        ranks-counts (frequencies ranks)
        ranks-counts-counts (frequencies (vals ranks-counts))
        n-pairs (get ranks-counts-counts 2)]
    (or (= 2 n-pairs) (four-of-a-kind? hand))))

(defn straight? [hand]
  (let [ranks-org (map rank hand)
        is-straight? (fn [ranks] (let [sorted (sort ranks)
                                       min-rank (apply min sorted)
                                       max-rank (apply max sorted)
                                       straight-for-cmp (range min-rank (+ 1 max-rank))]
                                   (= sorted straight-for-cmp)))]
    (or (is-straight? ranks-org) (is-straight? (replace {14 1} ranks-org)))))

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
        check (fn [[checker val]] (if (checker hand) val -1))]
    (apply max (map check checkers))))
