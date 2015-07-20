(ns p-p-p-pokerface)

(defn rank [card]
  (def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})
  (let [[card-rank _] card]
  (if (contains? replacements card-rank)
    (replacements card-rank)
    (Integer/valueOf (str card-rank)))))

(defn suit [card]
  (let [[_ card-suit] card]
    (str card-suit)))

(defn high-card? [hand]
  true)

(defn pair? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (== (apply max (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (= (sort (vals (frequencies (map rank hand)))) (seq [2 3])))

(defn two-pairs? [hand]
  (= (sort (vals (frequencies (map rank hand)))) (seq [1 2 2])))

(defn straight? [hand]
  (let [[sorted-hand lowest-rank] [
                                   (sort (map rank hand))
                                   (apply min (map rank hand))]]
    (if (= sorted-hand (range lowest-rank (+ lowest-rank 5)))
      true
      (if (= (sort (replace {14 1} sorted-hand)) (range 1 6))
        true
        false))))

(defn straight-flush? [hand]
  (and
   (straight? hand)
   (flush? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1]
                   [two-pairs? 2] [three-of-a-kind? 3]
                   [straight? 4] [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (map (fn [x] (if ((first x) hand) (second x) -1)) checkers))))
