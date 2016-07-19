(ns p-p-p-pokerface)

(def card-values
	{\1 1,
	 \2 2,
	 \3 3,
	 \4 4,
	 \5 5,
	 \6 6,
	 \7 7,
	 \8 8,
	 \9 9,
	 \T 10,
	 \J 11,
	 \Q 12,
	 \K 13,
	 \A 14})

(defn rank [card]
	(card-values (first card)))

(defn rank-frequencies [hand]
	(vals (frequencies (map rank hand))))

(defn suit [card]
	(str (second card)))

(defn pair? [hand]
	(not (empty? (filter #(< 1 %) (rank-frequencies hand)))))

(defn three-of-a-kind? [hand]
	(not (empty? (filter #(< 2 %) (rank-frequencies hand)))))

(defn four-of-a-kind? [hand]
	(not (empty? (filter #(< 3 %) (rank-frequencies hand)))))

(defn flush? [hand]
	(= 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
	(= [2 3] (sort (rank-frequencies hand))))

(defn two-pairs? [hand]
	(< 3 (apply + (filter #(< 1 %) (rank-frequencies hand)))))

(defn straight? [hand]
	(let [h (sort (map rank hand))
		  c (count (set h))
		  replacements {14 1}
		  h2 (sort (replace replacements h))
		  diff #(- (last %) (first %))
		  ]
		(and (= 5 c)
			 (or
				 (= 4 (diff h))
				 (= 4 (diff h2))))))

(defn straight-flush? [hand]
	(and
		(flush? hand)
		(straight? hand)))

(defn high-card? [hand]
	true) ; All hands have a high card.

(defn value [hand]
	(let [checkers #{[high-card? 0]  [pair? 1]
					 [two-pairs? 2]  [three-of-a-kind? 3]
					 [straight? 4]   [flush? 5]
					 [full-house? 6] [four-of-a-kind? 7]
					 [straight-flush? 8]}]
		(apply max (map second (filter #((first %) hand) checkers)))))
