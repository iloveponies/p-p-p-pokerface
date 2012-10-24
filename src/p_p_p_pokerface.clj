(ns p-p-p-pokerface)

(defn rank [card]
   (let [[fst snd] card]
     (if (Character/isDigit fst)
        (Integer/valueOf (str fst))
        (get {\T 10, \J 11, \Q 12, \K 13, \A 14} fst)
     )
   )
)

(defn suit [card]
   (let [[fst snd] card]
     (str snd)
   )
)

(defn pair? [hand]
   (contains? (set (vals (frequencies (map rank hand)))) 2)  
)

(defn two-pairs? [hand]
  (or (= (sort (vals (frequencies (map rank hand)))) [1 2 2])
      (= (sort (vals (frequencies (map rank hand)))) [1 4]))
)

(defn three-of-a-kind? [hand]
  (= (apply max (vals (frequencies (map rank hand)))) 3)
 )

(defn four-of-a-kind? [hand]
  (= (apply max (vals (frequencies (map rank hand)))) 4)
 )

(defn straight? [hand]
  (let [handrank (sort (map rank hand))]
  (or (= handrank (range (apply min handrank) (+ (apply max handrank) 1)))
      (= handrank [2 3 4 5 14]))
   )
)

(defn flush? [hand]
  (= (apply max (vals (frequencies (map suit hand)))) 5)
)

(defn full-house? [hand]
  (= (sort (vals (frequencies (map rank hand)))) (range 2 4))
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