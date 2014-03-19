(ns p-p-p-pokerface)

(defn rank [card]
  (let [higher-ranks {\T 10, \J 11, \Q 12, \K 13, \A 14}
        [rank suit] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (get higher-ranks rank)
      )
    )
  )

(defn suit [card]
  (let [[rank suite] card]
    (str suite)
    )
  )

(defn pair? [hand]
  (< 1 (apply max (vals (frequencies (map rank hand)))))
  )

(defn three-of-a-kind? [hand]
  (< 2 (apply max (vals (frequencies (map rank hand)))))
  )

(defn four-of-a-kind? [hand]
  (< 3 (apply max (vals (frequencies (map rank hand)))))
  )

(defn flush? [hand]
  (< 4 (apply max (vals (frequencies (map suit hand)))))
  )

(defn full-house? [hand]
  (and (= 3 (apply max (vals (frequencies (map rank hand)))))
  (= 2 (apply min (vals (frequencies (map rank hand))))))
  )

(defn two-pairs? [hand]
  (let [is-pair (fn [n] (= 2 n))]
    (if (four-of-a-kind? hand)
        true
        (= 2 (count (filter is-pair (vals (frequencies (map rank hand))))))
      )
    )
  )

(defn straight? [hand]
  (let [range-from (fn [n] (range n (+ n 5)))]
    (or (= (range-from (apply min (sort (map rank hand)))) (sort (map rank hand)) )
    (= (range-from (apply min (sort (replace {14, 1} (map rank hand))))) (sort (replace {14, 1} (map rank hand))) ))
    )
  )

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand))
  )

(defn high-card? [card]
  true
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
   (high-card? hand) 0
   )
  )
