(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card]
  (if (Character/isDigit fst)
    (Integer/valueOf (str fst))
    ({\T 10, \J 11, \Q 12, \K 13, \A 14} fst)
  ))
)

(defn suit [card]
  (let [[_ snd] card]
    (str snd))
)

(defn same-ranks [hand]
  (apply max (vals (frequencies (map rank hand))))
)

(defn same-suits [hand]
  (apply max (vals (frequencies (map suit hand))))
)

(defn pair? [hand]
  (if (>= (same-ranks hand) 2)
    true
    false
  )
)

(defn three-of-a-kind? [hand]
  (if (>= (same-ranks hand) 3)
    true
    false
  )
)

(defn four-of-a-kind? [hand]
  (if (>= (same-ranks hand) 4)
    true
    false
  )
)

(defn flush? [hand]
  (if (== (same-suits hand) 5)
    true
    false
  )
)

(defn full-house? [hand]
  (= (range 2 4) (sort (vals (frequencies (map rank hand)))))
)

(defn two-pairs? [hand]
  (let [freqs (sort (vals (frequencies (map rank hand))))]
    (cond
      (= freqs (seq [1 2 2])) true
      (= freqs (seq [1 4])) true
      :else false))
)

(defn straight? [hand]
  (let [sorted (sort (map rank hand))
        ranged (range (first sorted) (+ (first sorted) 5))]
    (cond
      (= sorted (seq [2 3 4 5 14])) true
      (= sorted ranged) true
      :else false))
)

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand))
)

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
    :else 0
  )
)






