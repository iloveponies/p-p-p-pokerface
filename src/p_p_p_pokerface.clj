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
  (
    let[
      plop (sort(map rank hand))
      niks (sort(replace {14 1} plop))
    ]
    ( or (= plop (range (apply min plop) (+ (apply max plop) 1))) (= niks (range (apply min niks) (+ (apply max niks) 1))))
  )
)

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand))
)

(defn high-card? [hand]
  true
)

(defn value [hand]
  (
    let [checkers
         #{[high-card? 0]  [pair? 1]
           [two-pairs? 2]  [three-of-a-kind? 3]
           [straight? 4]   [flush? 5]
           [full-house? 6] [four-of-a-kind? 7]
           [straight-flush? 8]}
        ]
  (apply max (map second (filter (fn[checker] ((first checker) hand)) checkers)))
  )
)
