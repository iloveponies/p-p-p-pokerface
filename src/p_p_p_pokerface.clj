(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank suit] card]
    (let [list {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (list rank)
      )
    )
  )
)

(defn suit [card]
  (let [[rank suit] card]
    (str suit)
    )
  )

(defn samecards [hand]
  (let [nums (map rank hand)]
    (apply max (vals (frequencies nums)))
  )
)

(defn pair? [hand]
  (= 2 (samecards hand))
 )

(defn three-of-a-kind? [hand]
  (= 3 (samecards hand))
)

(defn four-of-a-kind? [hand]
  (= 4 (samecards hand))
  )

(defn flush? [hand]
  (= 5 (apply max
    (vals (frequencies
    (map suit hand)))))
  )

(defn full-house? [hand]
  (let [nums (map rank hand)]
    (= [2 3] (sort (vals (frequencies nums))))
    )
  )

(defn two-pairs? [hand]
  (let [nums (map rank hand)]
    (or (= (sort (vals (frequencies nums))) [1 2 2])
        (= (sort (vals (frequencies nums))) [1 4]))
    )
  )

(defn straight? [hand]
  (let [nums (map rank hand)]
  (let [m (apply min nums)]
  (let [mace (apply min (replace {14 1} nums))]
    (or
    (= (sort nums) (range m (+ m 5)))
    (= (sort (replace {14 1} nums)) (range mace (+ mace 5)))
  )))))



(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand))
  )

(defn high-card? [hand]
  true)


(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]

  (apply max (map (fn [x]
    (if ((first x) hand)
        (second x)
        0))

  checkers))))





