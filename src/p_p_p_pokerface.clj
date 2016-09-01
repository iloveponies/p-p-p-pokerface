(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card]
	(if (Character/isDigit rank)
		(Integer/valueOf (str rank))
		(get {\T 10, \J 11, \Q 12, \K 13, \A 14} rank)
	)
  )
)

(defn suit [card]
  (let [[_ suit] card]
	(str suit))
)

(defn pair? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 2) 
)

(defn three-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (>= (apply max (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (= (sort (vals (frequencies (map rank hand)))) [2 3])
)

(defn two-pairs? [hand]
  (> (reduce + (filter #(> % 1) (vals (frequencies (map rank hand))))) 3)
)

(defn straight? [hand]
  (if (and (> (apply max (sort (map rank hand))) 13) (< (apply min (sort (map rank hand))) 3))
	(= (sort (concat (remove #(> % 13) (sort (map rank hand))) [1])) (range 1 6))
	(= (sort (map rank hand)) (range (apply min (sort (map rank hand))) (+ (apply min (sort (map rank hand))) 5)))
  )
)

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand))
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
