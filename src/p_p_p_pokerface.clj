(ns p-p-p-pokerface)

(defn suit [card]
  (let [[_ scnd] card]
    (str scnd))
  )

(defn rank [card]
  (let [[frst _] card
        faces {\T 10, \J 11, \Q 12, \K 13, \A 14}
        ]
  (if (Character/isDigit frst)
    (Integer/valueOf (str frst))
    (faces frst)
    ))
  )

(defn pair? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 2)
  )

(defn three-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 3)
  )

(defn four-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 4)
  )

(defn flush? [hand]
  (apply = (map suit hand))
  )

(defn full-house? [hand]
  (= [2 3] (sort (vals (frequencies (map rank hand)))))
  )

(defn two-pairs? [hand]
  (let [[freq-count] [(sort (vals (frequencies (map rank hand))))]]
    (or 
      (= [1 2 2] freq-count) 
      (= [1 4] freq-count)
      ))
  )

(defn straight? [hand]
  (let [[hand-ranks] [(sort (map rank hand))]
        [low-rank] [(first hand-ranks)]
        ]
    (or (= hand-ranks (range low-rank (+ 5 low-rank))) 
        (= hand-ranks [2 3 4 5 14]) ; special case for low ace straight
        )
  ))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)
       )
  )

(defn value [hand]
  (cond 
    (straight-flush? hand)  8
    (four-of-a-kind? hand)  7
    (full-house? hand)      6
    (flush? hand)           5
    (straight? hand)        4
    (three-of-a-kind? hand) 3
    (two-pairs? hand)        2
    (pair? hand)            1
    :else                   0
    )
  )





















