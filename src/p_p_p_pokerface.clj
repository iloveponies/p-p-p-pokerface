(ns p-p-p-pokerface)

(defn rank [card]
  (let
    [
      [rank _] card
      ranks {\T 10 \J 11 \Q 12 \K 13 \A 14}
    ]
    (if (Character/isDigit rank)
        (Integer/valueOf (str rank))
        (get ranks rank)
    )
  )
)

(defn suit [card]
  (let
    [[_ snd] card]
    (str snd)
  )
)

(defn n-of-a-kind? [hand n]
  (>= (apply max (vals (frequencies (map rank hand)))) n)
)

(defn pair? [hand]
  (n-of-a-kind? hand 2)
)

(defn three-of-a-kind? [hand]
  (n-of-a-kind? hand 3)
)

(defn four-of-a-kind? [hand]
  (n-of-a-kind? hand 4)
)

(defn flush? [hand]
  (= (count (frequencies (map suit hand))) 1)
)

(defn full-house? [hand]
  (if
    (and
      (not (four-of-a-kind? hand))
      (and (three-of-a-kind? hand) (pair? hand))
    )
    (= (count (frequencies (map rank hand))) 2)
    false
  )
)

(defn two-pairs? [hand]
  (let
    [counts (vals (frequencies (map rank hand)))
     freqs (filter (fn [freq] (>= freq 2)) counts)]
    (cond
      (four-of-a-kind? hand) true
      (== (count freqs) 2) true
      :else false
     )
   )
)

(defn straight? [hand]
  (let
    [
      sorted-ranks (sort (map rank hand))
      min-rank (apply min sorted-ranks)
      max-rank (apply max sorted-ranks)
      rank-range (range min-rank (+ max-rank 1))
      ace-involved (=(apply max sorted-ranks) 14)
    ]
    (if 
      (and (== (count rank-range) 5) (= rank-range sorted-ranks))
      true
      (if (boolean ace-involved) (= sorted-ranks [2 3 4 5 14]) false))
  )
)

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand))
)

(defn value [hand]
  (cond
    (straight-flush? hand)  8
    (four-of-a-kind? hand)  7
    (full-house? hand)      6
    (flush? hand)           5
    (straight? hand)        4
    (three-of-a-kind? hand) 3
    (two-pairs? hand)       2
    (pair? hand)            1
    :else                   0
  )
)
