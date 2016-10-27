(ns p-p-p-pokerface)

(def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
	(let [[n] card]
		(if (Character/isDigit n)
			(Integer/valueOf (str n))
			(replacements n)
		)
	)
)

(defn suit [card]
	(let [[_ s] card] (str s))
)



(defn pair? [hand]
	(= (apply max (vals (frequencies (map rank hand)))) 2)
)

(defn three-of-a-kind? [hand]
	(= (apply max (vals (frequencies (map rank hand)))) 3)
)

(defn four-of-a-kind? [hand]
	(= (apply max (vals (frequencies (map rank hand)))) 4)
)

(defn flush? [hand]
	(apply = (map suit hand))
)

(defn full-house? [hand]
	(= (sort (vals (frequencies (map rank hand)))) [2 3])
)

(defn two-pairs? [hand]
	(= (sort (vals (frequencies (map rank hand)))) [1 2 2])
)

(defn straight? [hand]
	(let
		[v_max (apply max (map rank hand))
		 v_min (apply min (map rank hand))
		 av_min (apply min (replace {14 1} (map rank hand)))
		 av_max (apply max (replace {14 1} (map rank hand)))
		 vls   (vals (frequencies (map rank hand)))]
		
		(and 
			(= (apply max vls) 1)
			(or
				(= (- av_max av_min) 4)
				(= (- v_max v_min) 4)
			)
		)
	)
)

(defn straight-flush? [hand]
	(and
		(straight? hand)
		(flush? hand)
	)
)

(defn high-card? [hand]
  true)
  
(defn value [hand]
	(let [checkers #{[pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
		
		(apply max (map (fn [x] (if ((first x) hand) (second x) 0)) checkers))
	)
)
