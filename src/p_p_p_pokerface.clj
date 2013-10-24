(ns p-p-p-pokerface)

(def ranks{
  \1 1
  \2 2
  \3 3
  \4 4
  \5 5
  \6 6
  \7 7
  \8 8
  \9 9
  \T 10
  \J 11
  \Q 12
  \K 13
  \A 14
  })

(def ranks2{
  \1 1
  \2 2
  \3 3
  \4 4
  \5 5
  \6 6
  \7 7
  \8 8
  \9 9
  \T 10
  \J 11
  \Q 12
  \K 13
  \A 1
  })

(defn rank [card]
  (get ranks (get card 0))
)

(defn rank2 [card]
  (get ranks2 (get card 0))
)

(defn suit [card]
  (str (get card 1))
)

(defn pair? [hand]
  (== 2 (apply max (vals (frequencies (map rank hand )))))
)
  

(defn three-of-a-kind? [hand]
  (== 3 (apply max (vals (frequencies (map rank hand )))))  
)

(defn four-of-a-kind? [hand]
  (== 4 (apply max (vals (frequencies (map rank hand )))))  
)

(defn flush? [hand]
  (== 5 (apply max (vals (frequencies (map suit hand )))))  
)

(defn full-house? [hand]
  (= #{2 3} (set (vals (frequencies (map rank hand )))))
)

(defn two-pairs? [hand]
  (= [1 2 2] (vec (sort (vals (frequencies (map rank hand ))))))
  )

(defn increments? [xs] (
    let [f (first xs)
         r (rest xs)
         fr (first (rest xs))]
    (cond 
      (empty? r) true
      :else (and (== (+ f 1) fr) (increments? r) )
      )
  )
)

(defn straight? [hand] (
  let [vals1 (map rank hand)
       vals2 (map rank2 hand)]
  (or (increments? (vec (sort (map rank2 hand))) ) 
      (increments? (vec (sort (map rank  hand))) ) 
  ))
) 

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand) ))

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
