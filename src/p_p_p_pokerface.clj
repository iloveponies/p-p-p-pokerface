(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (get {\T 10, \J 11, \Q 12, \K 13, \A 14} fst)
    )
  )
)

(defn suit [card]
  (let [[_ snd] card]
    (str snd)
  )
)

(defn pair? [hand]
  (if (= 2 (apply max (vals (frequencies (map rank hand)))))
    true
    false
  )
)

(defn three-of-a-kind? [hand]
  (if (= 3 (apply max (vals (frequencies (map rank hand)))))
    true
    false
  )
)

(defn four-of-a-kind? [hand]
  (if (= 4 (apply max (vals (frequencies (map rank hand)))))
    true
    false
  )
)

(defn flush? [hand]
  (if (= 5 (apply max (vals (frequencies (map suit hand)))))
    true
    false
  )
)

(defn full-house? [hand]
  (= [3 2] (vals (frequencies (map rank hand))))
)

(defn two-pairs? [hand]
  (= [2 2 1] (vals (frequencies (map rank hand))))
)

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)

