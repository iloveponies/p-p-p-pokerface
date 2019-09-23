(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst snd] card
        value {\T 10, \J 11, \Q 12, \K 13, \A 14}]
  (cond
   (Character/isDigit fst) (Integer/valueOf (str fst))
   :else                   (get value fst)
  )))

(defn suit [card]
  (let [[fst snd] card]
    (str snd)
  ))

(defn pair? [hand]
  (let [result (apply max(vals (frequencies (map rank hand))))]
  (> result 1)
  ))



(defn three-of-a-kind? [hand]
  (contains?
   (set (vals (frequencies (map rank hand))))
   3
  ))


(defn four-of-a-kind? [hand]
   (contains?
   (set (vals (frequencies (map rank hand))))
   4
  ))


(defn flush? [hand]
  (== (count(set(map suit hand))) 1)
  )


(defn full-house? [hand]
  (let [theHand (sort(vals(frequencies(map rank hand))))]
   (= theHand (range 2 4))
  ))




(defn two-pairs? [hand]
  (cond
   (four-of-a-kind? hand) true
   :else                  (= [1 2 2]
                          (sort(vals(frequencies(map rank hand)))))
   ))


(defn straight? [hand]
  (let[sortedHand (sort(map rank hand))
       minim (apply min sortedHand)
       maxim (apply max sortedHand)
       replaced (sort(replace {maxim 1} sortedHand))
       testWith (range minim (+ maxim 1))
       minimReplc (apply min replaced)
       maximReplc (apply max replaced)
       testWithReplc (range minimReplc (+ maximReplc 1))]
  (cond
   (= testWith sortedHand)    true
   (==  maxim 14) (= replaced testWithReplc)
   :else                      false
  )))



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
                 [straight-flush? 8]}
        filteredSet (filter #((first %) hand) checkers)
        secondValue (map second filteredSet)
        ]

  (apply max secondValue)

  ))





