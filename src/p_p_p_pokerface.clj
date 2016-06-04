(ns p-p-p-pokerface)

(defn rank [card]
  (let [[card-rank _] card]
    (if (Character/isDigit card-rank)
      (Integer/valueOf (str card-rank))
      (get { \T 10 \J 11 \Q 12 \K 13 \A 14} card-rank))))

(defn suit [card]
  (let [[_ card-suit] card]
    (str card-suit)))

(defn ranks [hand]
  (map rank hand))

(defn rank-occurences [hand]
  (frequencies (ranks hand)))

(defn rank-groups [hand]
  (vals (rank-occurences hand)))

(defn pair? [hand]
  (= 2 (apply max (rank-groups hand))))

(defn three-of-a-kind? [hand]
  (and
   (= 1 (apply min (rank-groups hand)))
   (= 3 (apply max (rank-groups hand)))))

(defn four-of-a-kind? [hand]
  (= 4 (apply max (rank-groups hand))))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (apply = suits)))

(defn full-house? [hand]
  (= '(2 3) (sort (rank-groups hand))))

(defn two-pairs? [hand]
  (= '(1 2 2) (sort (rank-groups hand))))

(defn straight? [hand]
  (let [sorted-ranks (sort (ranks hand))
        min-rank (apply min sorted-ranks)]
    (or (= '(2 3 4 5 14) sorted-ranks)
        (= (range min-rank (+ 5 min-rank)) sorted-ranks))))

(defn straight-flush? [hand]
  (and
   (straight? hand)
   (flush? hand)))

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
