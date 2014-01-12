(ns p-p-p-pokerface)

(defn rank [card]
  (let [ranks {\T 10, \J 11, \Q 12
               \K 13, \A 14}
        [rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (ranks rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn rank-frequencies [hand]
  (let [ranks (map rank hand)]
    (vals (frequencies ranks))))

(defn high-card? [hand]
  true)

(defn pair? [hand]
  (contains? (set (rank-frequencies hand)) 2))

(defn three-of-a-kind? [hand]
  (contains? (set (rank-frequencies hand)) 3))

(defn four-of-a-kind? [hand]
  (contains? (set (rank-frequencies hand)) 4))

(defn flush? [hand]
  (== (count (set (map suit hand))) 1))

(defn full-house? [hand]
  (or
    (= (rank-frequencies hand) [2 3])
    (= (rank-frequencies hand) [3 2])))

(defn two-pairs? [hand]
  (let [sorted-freqs (sort (rank-frequencies hand))]
    (or
      (= sorted-freqs [1 2 2])
      (= sorted-freqs [1 4]))))

(defn straight? [hand]
  (let [sorted-freqs
        (sort (map rank hand))
        sorted-freqs-ace-as-one
        (sort (replace {14 1} sorted-freqs))
        first-rank
        (first sorted-freqs)
        first-rank-ace-as-one
        (first sorted-freqs-ace-as-one)]
    (or
      (= sorted-freqs
         (range first-rank (+ first-rank 5)))
      (= sorted-freqs-ace-as-one
         (range first-rank-ace-as-one
                (+ first-rank-ace-as-one 5))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    :-))

