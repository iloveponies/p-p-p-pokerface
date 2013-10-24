(ns p-p-p-pokerface)

(def royals {\A 14, \K 13, \Q 12, \J 11, \T 10})

(defn rank [card]
  (let [[rank _] card]
    (if
      (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (royals rank))))


(defn suit [card]
 (let [[ranks suit] card]
   (str suit)))


(defn pair? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (>= (apply max (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (= [2 3] (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (= [1 2 2] (sort (vals (frequencies (map rank hand))))))

(defn straight? [hand]
  (let [suurin (apply max (map rank hand))
        gordid (sort (map rank hand))
        pienin (apply min (map rank hand))]
    (if
      (= suurin 14)
      (cond
       (= (range 10 15) gordid) true
       (= [2 3 4 5 14] gordid) true
       :else false)
      (= (range pienin (+ pienin 5)) gordid)))
  )

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

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
   :else 0))
