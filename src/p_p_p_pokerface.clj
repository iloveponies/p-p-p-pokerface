(ns p-p-p-pokerface)

(defn rank [card]
    (let [[rank suit] card]
        (if (Character/isDigit rank)
            (Integer/valueOf (str rank))
            ({\T 10, \J 11, \Q 12, \K 13, \A 14} rank)
        )
    )
)

(defn suit [card]
    (let [[_ suit] card]
        (str suit))
)

(defn pair? [hand]
    (and
    (contains? (set (vals (frequencies (map rank hand)))) 2)
    (not (contains? (set (vals (frequencies (map rank hand)))) 3))
    (not (contains? (set (vals (frequencies (map rank hand)))) 4))
    (not (contains? (set (vals (frequencies (map rank hand)))) 5))
    )
)

(defn three-of-a-kind? [hand]
    (and
    (contains? (set (vals (frequencies (map rank hand)))) 3)
    (not (contains? (set (vals (frequencies (map rank hand)))) 2))
    (not (contains? (set (vals (frequencies (map rank hand)))) 4))
    (not (contains? (set (vals (frequencies (map rank hand)))) 5))
    )
)

(defn four-of-a-kind? [hand]
    (and
    (contains? (set (vals (frequencies (map rank hand)))) 4)
    (not (contains? (set (vals (frequencies (map rank hand)))) 2))
    (not (contains? (set (vals (frequencies (map rank hand)))) 3))
    (not (contains? (set (vals (frequencies (map rank hand)))) 5))
    )
)

(defn flush? [hand]
    (contains? (set (vals (frequencies (map suit hand) ))) 5)
)

(defn full-house? [hand]
    (and
    (contains? (set (vals (frequencies (map rank hand)))) 2)
    (contains? (set (vals (frequencies (map rank hand)))) 3)
    )
)

(defn two-pairs? [hand]
    (if (= 2 (get (frequencies (vals (frequencies (map rank hand)))) 2))
        true
        false
    )
)

(defn straight? [hand]
    (let [ranks (sort (map rank hand))
          minval (apply min ranks)
          maxval (apply max ranks)]
    
    (if (and (= minval 2) (= maxval 14) )
        (if (= (sort (conj (range 2 6) 14)) ranks)
            true
            false
        )
        (if (= (range minval (+ minval 5)) ranks)
            true
            false
        )
    ))
)

(defn straight-flush? [hand]
    (and (straight? hand) (flush? hand))
)

(defn value [hand]
    (cond
    (straight-flush? hand)  8
    (straight? hand)        4
    (two-pairs? hand)       2
    (full-house? hand)      6
    (flush? hand)           5
    (four-of-a-kind? hand)  7
    (three-of-a-kind? hand) 3
    (pair? hand)            1
    :else                   0
    )
)
