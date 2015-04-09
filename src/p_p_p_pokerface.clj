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
  nil)

(defn two-pairs? [hand]
  nil)

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
