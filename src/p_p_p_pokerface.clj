(ns p-p-p-pokerface)

(def replacement { \T 10, \J 11, \Q 12, \K 13, \A 14} )

(defn rank [card]
  (let [ [rank] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank) )
      (replacement rank)
    )
  )
)

(defn suit [card]
  (let [ [rank suit] card]
    (str suit) 
  )
 )

(defn pair? [hand]
  (let [nums (map rank hand)
        freq (vals (frequencies nums) )]

    (= (apply max freq) 2)
  )
)

(defn three-of-a-kind? [hand]
  (let [nums (map rank hand)
        freq (vals (frequencies nums) )]

    (= (apply max freq) 3)
  )
)

(defn four-of-a-kind? [hand]
  (let [nums (map rank hand)
        freq (vals (frequencies nums) )]

    (= (apply max freq) 4)
  )
)

(defn flush? [hand]
  (let [ [first second third fourth fifth] hand ]
  
    (= (suit first) (suit second) (suit third) (suit fourth) (suit fifth) )
  )
)

(defn full-house? [hand]
  (let [nums (map rank hand)
        freq (sort (vals (frequencies nums) ) ) ]
    
    (and (= (first freq) 2) (= (first (rest freq)) 3) ) 
  )
)

(defn two-pairs? [hand]
  (let [nums (map rank hand)
        freq (sort (vals (frequencies nums) ) ) ]
    
    (and (= (first freq) 1) (= (first (rest freq) ) 2) (= (first (rest (rest freq) )) 2 ) ) 
  )
)

(defn straight? [hand]
  (let [nums (sort (map rank hand) )
        [frst second third fourth fifth] nums
        eka (first nums) ]

    (if (= fifth 14)
      (or (= [frst second third fourth] [2 3 4 5] ) (= nums (range eka (+ eka 5) ) ) )
      (= nums (range eka (+ eka 5) ) )
    )
  )
)

(defn straight-flush? [hand]

  (and (straight? hand) (flush? hand) )
)

(defn high-card? [hand]
  true
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
    :else		    0
  )
)
