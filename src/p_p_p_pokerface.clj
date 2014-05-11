(ns p-p-p-pokerface)

(defn rank [card]
  (let [highger_ranks {\T 10  \J 11 \Q 12  \K 13 \A 14}
        rank (get card 0 )
        ]
    (if (Character/isDigit rank)
     (Integer/valueOf (str rank))
     (get highger_ranks rank) )
    ))

(defn suit [card]
  (str (get card 1)))

(defn pair? [hand]
   (contains? (set (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
   (contains? (set (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
   (contains? (set (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (if (= 1 (count (set (map suit hand ))))
    (let [sort-ranks (sort (map rank hand))
          min-rank (apply min sort-ranks)
          max-rank (apply max sort-ranks)
          ]
      (not (= sort-ranks (range min-rank (+ 1 max-rank))))
      ) 
    false
    )
  )
    
(defn full-house? [hand]
  (= #{3,2} (set (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (let [sort-hand (sort (map rank hand))
        [r1 r2 r3 r4] sort-hand
        ]
    (and (= r1 r2) (= r3 r4))
    )
  )

(defn straight? [hand]
  (if (>= (count (frequencies (map suit hand))) 2)
    (let [ranks (map rank hand)
          ranks-ace-one (replace {14 1} ranks)
          sort-hand (fn [x] (sort > x))
          menos-uno (fn [a b c d e] (= 1 (- a  b) (- b c) (- c d ) (- d e)))
          ]
      (or (apply menos-uno (sort-hand ranks)) (apply menos-uno (sort-hand ranks-ace-one)))
      )
    false
    ))

(defn straight-flush? [hand]
  (if (= 1 (count (frequencies (map suit hand))))
    (let [ranks (map rank hand)
          ranks-ace-one (replace {14 1} ranks)
          sort-hand (fn [x] (sort > x))
          menos-uno (fn [a b c d e] (= 1 (- a  b) (- b c) (- c d ) (- d e)))
          ]
      (or (apply menos-uno (sort-hand ranks)) (apply menos-uno (sort-hand ranks-ace-one)))
      )
    false
    ))

(defn high-card?  [hand]
    true) ; All hands have a high card.

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (map second (filter #((first %) hand) checkers )))
    ))

