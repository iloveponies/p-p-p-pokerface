(ns p-p-p-pokerface)

(defn suit [card]
  (let [ [_ s] card
         ]
    (str s))
  )

(defn rank [card]
  (let [rank-map {\T 10 \J 11 \Q 12 \K 13 \A 14}
        [r _] card ]
    ( if (Character/isDigit r)
      (Integer/valueOf (str r))
      (rank-map r))
    )
  )

(defn pair? [hand]
  (> (apply max (vals (frequencies (map rank hand) ) ) ) 1)
  )

(defn three-of-a-kind? [hand]
  (> (apply max (vals (frequencies (map rank hand)))) 2)
  )

(defn four-of-a-kind? [hand]
  (> (apply max (vals (frequencies (map rank hand)))) 3)
  )

(defn flush? [hand]
  (== (apply max (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (= (range 2 4)
     (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (>= (apply + (filter (fn [x] (>= x 2)) (vals (frequencies (map rank hand))))) 4)
  )

(defn  straight? [hand]
  (let [ace-low {14 1}
        sorted-hand-ranks (sort (map rank hand))
        hand-min (first sorted-hand-ranks)
        sorted-ace-low-hand-ranks (sort (replace ace-low (map rank hand)))
        ace-low-hand-min (first sorted-ace-low-hand-ranks )
        hand-size 5]
    (cond
     (= (range hand-min (+ hand-min hand-size)) sorted-hand-ranks) true
     (= (range ace-low-hand-min (+ ace-low-hand-min hand-size)) sorted-ace-low-hand-ranks) true
     :else false
     )
  )
  )

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

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
   :else 0)
 )
