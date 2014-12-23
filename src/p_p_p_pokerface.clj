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

(defn has-rank-with-arity? [hand arity]
  (let [rank-counts (vals (frequencies (map rank hand)))]
    (<= arity (apply max rank-counts))))

(defn pair? [hand]
  (has-rank-with-arity? hand 2))

(defn three-of-a-kind? [hand]
  (has-rank-with-arity? hand 3))

(defn four-of-a-kind? [hand]
  (has-rank-with-arity? hand 4))

(defn flush? [hand]
  nil)

(defn full-house? [hand]
  nil)

(defn two-pairs? [hand]
  nil)

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
