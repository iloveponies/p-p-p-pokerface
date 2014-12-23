(ns p-p-p-pokerface)

(def rank-values {\T 10
                  \J 11
                  \Q 12
                  \K 13
                  \A 14})

(defn rank [card]
  (let [[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (rank-values rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn rank-counts [hand]
  (vals (frequencies (map rank hand))))

(defn has-rank-with-arity? [hand arity]
  (<= arity (apply max (rank-counts hand))))

(defn pair? [hand]
  (has-rank-with-arity? hand 2))

(defn three-of-a-kind? [hand]
  (has-rank-with-arity? hand 3))

(defn four-of-a-kind? [hand]
  (has-rank-with-arity? hand 4))

(defn flush? [hand]
  (== 1 (count (set (map suit hand)))))

(defn full-house? [hand]
  (let [rank-count-set (set (rank-counts hand))]
    (and
      (contains? rank-count-set 2)
      (contains? rank-count-set 3))))

(defn two-pairs? [hand]
  nil)

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
