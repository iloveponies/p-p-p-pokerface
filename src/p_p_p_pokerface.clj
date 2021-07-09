(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card
        ranks {\T 10,
               \J 11,
               \Q 12,
               \K 13,
               \A 14, }] 
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (ranks rank)))
  )

(defn suit [card]
  (let [[_ suit] card]
    (str suit))
  )

(defn pair? [hand]
  (let [ranks (map rank hand)]
    (if (<= 2 (apply max (vals (frequencies ranks))))
      true
      false)
    )
  )

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (if (<= 3 (apply max (vals (frequencies ranks))))
      true
      false)
    )
  )

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (if (<= 4 (apply max (vals (frequencies ranks))))
      true
      false)
    )
  )

(defn flush? [hand]
  (let [suits (map suit hand)]
    (if (= 5 (apply max (vals (frequencies suits))))
      true
      false)
    )
  )

(defn full-house? [hand]
  (let [ranks (map rank hand)
        full (sort (range 2 4))]
    (if (= full (sort (vals (frequencies ranks))))
      true
      false)
    )
  )

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        val-freqs (vals (frequencies ranks))]
    (if (or (= val-freqs '(4 1)) (= val-freqs '(2 2 1)))
      true
      false)
    )
  )

(defn straight? [hand]
  (let [ranks (map rank hand)
        lowest (apply min ranks)
        highest (apply max ranks)
        low-ace [2 3 4 5 14]]
    (if (= (range lowest (inc highest)) (sort ranks) )
           true
           (if (= (sort ranks) low-ace )
             true
             false))
    )
  )

(defn straight-flush? [hand]
  (let [straight (straight? hand)
        is-flush (flush? hand)]
    (if (= true straight is-flush)
      true
      false)
    )
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
