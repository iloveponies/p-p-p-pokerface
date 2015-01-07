(ns p-p-p-pokerface)

(defn rank [card]
  (def rank-codes {\T 10
                   \J 11
                   \Q 12
                   \K 13
                   \A 14})
  (let [[r _] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (rank-codes r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn rank-frequencies [hand]
  (vals (frequencies (map rank hand))))

(defn same-kind-count [hand]
  (apply max (rank-frequencies hand)))

(defn pair? [hand]
  (> (same-kind-count hand) 1))

(defn three-of-a-kind? [hand]
  (= (same-kind-count hand) 3))

(defn four-of-a-kind? [hand]
  (= (same-kind-count hand) 4))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (= (sort (rank-frequencies hand)) [2 3]))

(defn two-pairs? [hand]
  (or
   (four-of-a-kind? hand)
   (= (sort (rank-frequencies hand)) [1 2 2])))

(defn are-ranks-consecutive? [ranks]
  (= ranks
     (let [min-rank (apply min ranks)]
       (range min-rank (+ min-rank 5)))))

(defn straight? [hand]
  (let [sorted-ranks (sort (map rank hand))
        sorted-ranks-alt (sort (replace {14 1} sorted-ranks))]
    (or (are-ranks-consecutive? sorted-ranks)
        (are-ranks-consecutive? sorted-ranks-alt))))

(defn straight-flush? [hand]
  (and (flush? hand)
       (straight? hand)))

(defn high-card? [hand]
  (not (and (pair? hand)
         (straight? hand)
         (straight-flush? hand))))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
;    (apply max (map (fn [[f v]] (if (f hand) v 0)) checkers))))
    (apply max (map second (filter (fn [x] ((first x) hand)) checkers)))))
