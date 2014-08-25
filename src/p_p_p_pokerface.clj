(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card]
    (let [rank-mapping {\T 10 \J 11 \Q 12 \K 13 \A 14}]
      (Integer/valueOf (if (Character/isDigit rank)
                         (str rank)
                         (rank-mapping rank))))))

(defn ranks [hand]
  (map rank hand))

(defn suit [card]
  (str (second card)))

(defn suits [hand]
  (map suit hand))

(defn most-frequent [coll]
  (apply max (vals (frequencies coll))))

(defn pair? [hand]
 (= (most-frequent (ranks hand)) 2))

(defn three-of-a-kind? [hand]
  (= (most-frequent (ranks hand)) 3))

(defn four-of-a-kind? [hand]
  (= (most-frequent (ranks hand)) 4))

(defn flush? [hand]
  (= (most-frequent (suits hand)) 5))

(defn rank-frequencies [hand]
  (sort (vals (frequencies (ranks hand)))))

(defn full-house? [hand]
  (= (rank-frequencies hand) [2 3]))

(defn two-pairs? [hand]
  (= (rank-frequencies hand) [1 2 2]))

(defn straight? [hand]
  (let [sorted-ranks (sort (ranks hand))
        first-sorted-rank (first sorted-ranks)
        sorted-ranks-ace (sort (replace {14 1} (ranks hand)))
        straight (fn [first-rank] (range first-rank (+ first-rank 5)))]
    (or (= sorted-ranks (straight first-sorted-rank)) (= sorted-ranks-ace (straight 1)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand] true)

(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1]
                   [two-pairs? 2] [three-of-a-kind? 3]
                   [straight? 4] [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        matching-checkers (filter #((first %1) hand) checkers)
        matching-values (map #(second %1) matching-checkers)]
    (apply max matching-values)))
