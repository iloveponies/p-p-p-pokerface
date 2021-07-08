(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get {\T 10
            \J 11
            \Q 12
            \K 13
            \A 14} r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn hand-values [hand]
  (let [ranks (map rank hand)]
    (vals (frequencies ranks))))

(defn pair? [hand]
  (contains? (set (hand-values hand)) 2))

(defn three-of-a-kind? [hand]
  (contains? (set (hand-values hand)) 3))

(defn four-of-a-kind? [hand]
  (contains? (set (hand-values hand)) 4))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (and
   (pair? hand)
   (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (= (sort (hand-values hand)) [1 2 2]))

(defn straight? [hand]
  (let [unsorted-ranks (map rank hand)
        ranks (sort unsorted-ranks)
        ranks' (sort (replace {14 1} ranks))
        min-rank (first ranks)
        min-rank' (first ranks')]
    (or
     (= ranks (range min-rank (+ min-rank 5)))
     (= ranks' (range min-rank' (+ min-rank' 5))))))

(defn straight-flush? [hand]
  (and
   (flush? hand)
   (straight? hand)))

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
