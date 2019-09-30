(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card
	      high-rank-map {\T 10 \J 11 \Q 12 \K 13 \A 14}]
	 (if (Character/isDigit r)
		   (Integer/valueOf (str r))
			 (get high-rank-map r))))

(defn suit [card]
  (let [[_ s] card]
	 (str s)))

(defn pair? [hand]
  (== 2 (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (== 3 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (== 4 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (= '(2 3) (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (= '(1 2 2) (sort (vals (frequencies (map rank hand))))))

(defn straight? [hand]
 (let [lowest (apply min (map rank hand))
       ranks (sort (map rank hand))
			 norm-ranks (map (fn [r] (- r lowest)) ranks)]
  (or (= '(0 1 2 3 4) norm-ranks) (= '(0 1 2 3 12) norm-ranks))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand] true)

(defn value [hand]
  (let [checkers #{[high-card? 0]   [pair? 1]
	                 [two-pairs? 2]  [three-of-a-kind? 3]
	                 [straight? 4]   [flush? 5]
	                 [full-house? 6] [four-of-a-kind? 7]
	                 [straight-flush? 8]}]
    (apply max (map second (filter (fn [[check _]] (check hand)) checkers)))))
