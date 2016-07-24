(ns p-p-p-pokerface)

(def face-card-ranks {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (get face-card-ranks rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (let [rank-counts (frequencies (map rank hand))]
    (= 2 (apply max (vals rank-counts)))))

(defn three-of-a-kind? [hand]
  (let [rank-counts (frequencies (map rank hand))]
    (= 3 (apply max (vals rank-counts)))))

(defn four-of-a-kind? [hand]
  (let [rank-counts (frequencies (map rank hand))]
    (= 4 (apply max (vals rank-counts)))))

(defn flush? [hand]
  (let [suits (set (map suit hand))]
    (= 1 (count suits))))

(defn full-house? [hand]
  (let [rank-counts (frequencies (map rank hand))]
    (= (range 2 4) (sort (vals rank-counts)))))

(defn two-pairs? [hand]
  (let [rank-counts (frequencies (map rank hand))]
    (= 2 (count (filter (fn [x] (= x 2)) (vals rank-counts))))))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        max-rank (apply max ranks)
        is-monotonic? (fn [ranks] (apply = 1 (map - (rest ranks) ranks)))]
    (if (= 14 max-rank)
        (or (is-monotonic? ranks) (is-monotonic? (sort (replace {14 1} ranks))))
        (is-monotonic? ranks))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn value [hand]
    (let [checkers #{[high-card? 0]  [pair? 1]
                     [two-pairs? 2]  [three-of-a-kind? 3]
                     [straight? 4]   [flush? 5]
                     [full-house? 6] [four-of-a-kind? 7]
                     [straight-flush? 8]}
          checker-values (map (fn [[checker value]] (if (checker hand) value 0)) checkers)]
          (apply max checker-values)))
