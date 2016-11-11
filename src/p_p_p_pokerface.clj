(ns p-p-p-pokerface)

(def high-cards {\A 14, \K 13, \Q 12, \J 11, \T 10})

(defn rank [card]
  (let [[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (high-cards rank))))

(defn rank-counts [hand]
  (let [ranks (map rank hand)]
    (vals (frequencies ranks))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (contains? (set (rank-counts hand)) 2))

(defn three-of-a-kind? [hand]
  (contains? (set (rank-counts hand)) 3))

(defn four-of-a-kind? [hand]
  (contains? (set (rank-counts hand)) 4))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (= 1 (count (set suits)))))

(defn full-house? [hand]
  (and (pair? hand)
       (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (= (sort (rank-counts hand))
      [1 2 2]))

(defn sequential-ranks? [ranks]
  (let [min-rank (apply min ranks)
        sorted (sort ranks)
        straight (range min-rank (+ 5 min-rank))]
    (= sorted straight)))

(defn straight? [hand]
  (let [ranks (map rank hand)]
    (or
      (sequential-ranks? ranks)
      (sequential-ranks? (replace {14 1} ranks)))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

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
                   [straight-flush? 8]}]
    (apply max (map second (filter (fn [x] ((first x) hand)) checkers)))))
