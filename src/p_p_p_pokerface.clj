(ns p-p-p-pokerface)

(defn rank [card]
  (let [[frst _] card]
  (if (Character/isDigit frst)
    (Integer/valueOf (str frst))
    (cond
     (= (str frst) "T") 10
     (= (str frst) "J") 11
     (= (str frst) "Q") 12
     (= (str frst) "K") 13
     (= (str frst) "A") 14
     )
    )
  )
 )

(defn max-rank [hand]
  (apply max (vals (frequencies (map rank hand))))
 )

(defn rank-freqs [hand]
  (vals (frequencies (map rank hand)))
 )


(defn suit [card]
  (let [[_ scnd] card]
  (str scnd))
  )

(defn count-suit-freq [hand]
  (count (vals (frequencies (map suit hand))))
 )


(defn pair? [hand]
  (if (>
       (max-rank hand)
       1)
    true
    false
   )
 )

(defn three-of-a-kind? [hand]
  (if (>
       (max-rank hand)
       2)
    true
    false
   )
  )

(defn four-of-a-kind? [hand]
  (if (>
       (max-rank hand)
       3)
    true
    false
   )
  )

(defn flush? [hand]
  (if (=
       (count-suit-freq hand)
       1)
   true
   false)
 )

(defn full-house? [hand]
   (if (and
       (== 2 (count (rank-freqs hand)))
       (== 2 (apply min (rank-freqs hand)))
        )
    true
    false)
  )

(defn two-pairs? [hand]
  (if (and
       (== 3 (count (rank-freqs hand)))
       (== 2 (apply max (rank-freqs hand)))
        )
    true
    false)
  )

(defn straight? [hand]
  (let [sorted-ranks (sort (map rank hand))
        sorted-rep-ranks (sort (replace {14, 1} sorted-ranks))]
      (if (or
            (= sorted-ranks (range (first sorted-ranks) (+ (first sorted-ranks) 5)))
            (and
             (= (last sorted-ranks) 14)
             (= sorted-rep-ranks
                (range (first sorted-rep-ranks)
                       (+ (first sorted-rep-ranks) 5)))
             )
           )
        true
        false
        )
   )
 )

(defn straight-flush? [hand]
  (and
   (straight? hand)
   (flush? hand))
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
