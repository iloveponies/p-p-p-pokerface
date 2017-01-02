(ns p-p-p-pokerface)

(defn rank [card]
 (let [[temp _] card muunnos {\T 10, \J 11, \Q 12, \K 13, \A 14}]
   (if (Character/isDigit temp)
      (Integer/valueOf (str temp))
      (get muunnos temp)))




)

(defn suit [card]
  (let [[_ temp] card]
    (str temp))
)

(defn pair? [hand]

  (= (apply max(vals (frequencies (map rank hand)))) 2)



)

(defn three-of-a-kind? [hand]

  (= (apply max(vals (frequencies (map rank hand)))) 3)

)

(defn four-of-a-kind? [hand]

  (= (apply max(vals (frequencies (map rank hand)))) 4)



)

(defn flush? [hand]
  ;;A flush is a poker hand containing five cards all of the same suit, not all of sequential rank -wiki

  (= (apply max(vals (frequencies (map suit hand)))) 5)

)

(defn full-house? [hand]
  ;;(and (pair? hand) (three-of-a-kind? hand))
  (=  (sort(vals (frequencies (map rank hand)))) (seq [2 3]))
)

(defn two-pairs? [hand]
  (let [funktio  (sort(vals (frequencies (map rank hand))))]
   (or(=  funktio (seq [1 2 2]))(=  funktio (seq [1 4])))
  )
)

(defn straight? [hand]
 (let [temp (sort (map rank hand)) aces (sort (replace {14 1} temp))]
   (or(= temp (range (first temp) (+ (first temp) 5)))
      (= aces (range (first aces) (+ (first aces) 5)))
     )
 )
)

(defn straight-flush? [hand]

 (and (straight? hand) (flush? hand))

)

(defn value [hand]
 (cond (straight-flush? hand)	8
  (four-of-a-kind?  hand)	 	7
  (full-house?  hand)		6
  ( flush?  hand)		5
  ( straight?  hand)		4
  ( three-of-a-kind?	 hand)	3
  ( two-pairs?  hand)		2
  ( pair?  hand)		1
  :else 0
 )
)
