(ns p-p-p-pokerface)

(defn rank [card]
  (
     let [[fst _] card]
     (if (Character/isDigit fst)
       (Integer/valueOf (str fst))
       ({\T 10, \J 11, \Q 12, \K 13, \A 14} fst)
     )
  )
)

(defn suit [card]
  (
    let [[_ snd] card]
    (str snd)
  )
)

(defn helper [hand degree kind]
  (
    > (apply max (vals (frequencies (map kind hand)))) degree
  )
)

(defn pair? [hand]
  (helper hand 1 rank)
)

(defn three-of-a-kind? [hand]
  (helper hand 2 rank)
)

(defn four-of-a-kind? [hand]
  (helper hand 3 rank)
)

(defn flush? [hand]
    (helper hand 4 suit)
)

(defn full-house? [hand]
  ( = (sort(vals (frequencies (map rank hand)))) (seq [2,3]))
)

(defn two-pairs? [hand]
  ( or ( = ( rest ( sort( vals (frequencies (map rank hand))))) (seq [2,2])) (four-of-a-kind? hand) (full-house? hand) )
)

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
