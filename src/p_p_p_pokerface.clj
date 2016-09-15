(ns p-p-p-pokerface)

(def replacement {\T 10 \J 11 \Q 12 \K 13 \A 14})


(defn suit [card]
  (let [[_ val] card]
  (str val)    )
  )

(defn rankstring [card]
  (let [[val _] card]
  val    )
  )

(defn rank [card]
(let [r (rankstring card)]
  (cond
    (Character/isDigit r) (Integer/valueOf (str r))
    :else (get replacement r)
    )
  )
  )

(defn pair? [hand]
(== (apply max (vals (frequencies (map rank hand)
  ))) 2))

(defn three-of-a-kind? [hand]
(== (apply max (vals (frequencies (map rank hand)
  ))) 3))


(defn four-of-a-kind? [hand]
(== (apply max (vals (frequencies (map rank hand)
  ))) 4))


(defn flush? [hand]
(== (apply max (vals (frequencies (map suit hand)
  ))) 5))


(defn full-house? [hand]
(and (== (apply min (vals (frequencies (map rank hand)
  ))) 2) (== (apply max (vals (frequencies (map rank hand)
  ))) 3)))

(defn two-pairs? [hand]
(or (= [1 4] (sort (vals (frequencies (map rank hand)))
  )) (= [1 2 2] (sort (vals (frequencies (map rank hand)))
  )))
  )

(def replacementlower {\T 10 \J 11 \Q 12 \K 13 \A 1})

(defn ranklower [card]
(let [r (rankstring card)]
  (cond
    (Character/isDigit r) (Integer/valueOf (str r))
    :else (get replacementlower r)
    )
  )
)

(defn straight-upper? [hand]
(and (= (count (frequencies (map rank hand))) 5) (= (sort (keys (frequencies (map rank hand)))) (range (apply min (sort (keys (frequencies (map rank hand))))) (+ (apply max (sort (keys (frequencies (map rank hand))))) 1)))
  ))

(defn straight-lower? [hand]
(and (= (count (keys (frequencies (map ranklower hand)))) 5) (= (sort (keys (frequencies (map ranklower hand)))) (range (apply min (sort (keys (frequencies (map ranklower hand))))) (+ (apply max (sort (keys (frequencies (map ranklower hand))))) 1)))
  ))

(defn straight? [hand]
  (or (straight-lower? hand) (straight-upper? hand))
)

(defn straight-flush? [hand]
(and (straight? hand) (flush? hand))
  )

(defn high-card? [hand]
  true)

(defn value [hand]
 (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
 (apply max (map second (filter (fn [[function _]] (function hand)) checkers)))
   )
)
