(ns p-p-p-pokerface)

(defn rank [card]
   (let [[first snd] card]
    (if(Character/isDigit first)(Integer/valueOf(str first))

           (get {\T 10 \J 11 \Q 12 \K 13 \A 14}  first)
      )

     ))

(defn suit [card]
  (let [[_ snd] card]
  (str snd)))

(defn pair? [hand]
  ( = (apply max (vals (frequencies(mapv rank hand)))) 2)

  )

(defn three-of-a-kind? [hand]

   ( = (apply max (vals (frequencies(mapv rank hand)))) 3)
  )


(defn four-of-a-kind? [hand]
   ( = (apply max (vals (frequencies(mapv rank hand)))) 4)


  )

(defn flush? [hand]
 (and ( = (apply max (vals (frequencies(mapv suit hand))))
    (count hand)
    ) (not (=(range (apply min
                 (mapv rank hand))
               (apply max
                 (mapv rank hand)))
        (sort (mapv rank hand))))
    )
  )

(defn full-house? [hand]
  (let [counts  (vals (frequencies(mapv rank hand)))]
    (=(sort counts)(range 2 4))

  ))

(defn two-pairs? [hand]
   (let [counts  (vals (frequencies(mapv rank hand)))]
   (or (=(sort counts)[ 1 2 2])
    (=(sort counts)[ 1 4]))
  ))

(defn straight? [hand]
  ( let [sorted-rank1 (sort (mapv rank hand))

      sorted-rank2 (sort (replace {14 1} (mapv rank hand)))
      min1 (apply min sorted-rank1 )
      max1 (apply max sorted-rank1 )
      min2 (apply min sorted-rank2 )
      max2 (apply max sorted-rank2 )
         ]
    (or (= sorted-rank1 (range min1 (+ max1 1)))
    (= sorted-rank2 (range min2 (+ max2 1)))

    )
   )
  )
(defn straight-flush? [hand]
  (and (straight? hand ) (flush? hand) )
)

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        values (for [check checkers]  (if (  (first check) hand ) (second check) 0 ))
        ]
    (apply max  values )

    )
  )
