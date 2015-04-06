(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card ]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (get {\T 10, \J 11 \Q 12 \K 13 \A 14} rank))))

(defn suit [card]
   (let [[_ suit] card]
     (str suit)))

(defn rank_vals [hand]
  (vals (frequencies (map rank hand))))

(defn sorted_rank_vals [hand]
  (sort (rank_vals hand)))

(defn pair? [hand]
  (== (apply max (rank_vals hand)) 2))

(defn three-of-a-kind? [hand]
  (== (apply max (rank_vals hand)) 3))

(defn four-of-a-kind? [hand]
  (== (apply max (rank_vals hand)) 4))

(defn flush? [hand]
  (== (count (frequencies (map suit hand))) 1))

(defn full-house? [hand]
  (= [2 3] (sorted_rank_vals hand)))

(defn two-pairs? [hand]
  (= [1 2 2] (sorted_rank_vals hand)))

(defn rank_is? [card value]
  (== (rank card) value))

(defn check_straight [ranks]
  (= ranks (range (first ranks) (+ (first ranks) 5)))
)

(defn straight? [hand]
  (let [high_rank (sort (map rank hand))
        low_rank (sort (replace {14 1} (map rank hand)))]
    (or
      (= high_rank (range (first high_rank) (+ (first high_rank) 5)))
      (= low_rank (range (first low_rank) (+ (first low_rank) 5))))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn value [hand]
  (cond
   (straight-flush? hand) 8
   (four-of-a-kind? hand) 7
   (full-house? hand) 6
   (flush? hand) 5
   (straight? hand) 4
   (three-of-a-kind? hand) 3
   (two-pairs? hand) 2
   (pair? hand) 1
   :else 0))

