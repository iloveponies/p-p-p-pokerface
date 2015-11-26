(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst snd] card]
    (if (Character/isDigit fst)
     (Integer/valueOf (str fst))
      ( get {\A 14, \K 13, \Q 12, \J 11} fst))
    ))

(defn suit [card]
  (let [[fst snd] card]
    (str snd)))

(defn pair? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 2)
  )

(defn three-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 3)
  )

(defn four-of-a-kind? [hand]
  nil)

(defn flush? [hand]
  nil)

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
