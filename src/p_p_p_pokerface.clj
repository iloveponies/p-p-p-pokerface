(ns p-p-p-pokerface)

(def ranks {\A 14, \T 10, \Q 12, \J 11, \K 13})
(defn rank [card]
  (let [[r _] card]
	(if (Character/isDigit r)
		(Integer/valueOf (str r))
		(get ranks r))))


(defn suit [card]
  (let [[_ s] card]
	(str s)))

(defn pair? [hand]
  (let [ hand-seq (seq hand)
	 x (map rank hand-seq)
	 freq (frequencies x)
	 values (vals freq)
	 filters (filter (fn [x] (= x 2)) values)]
	 (if (empty? filters)
		false
		true)))
	
(defn three-of-a-kind? [hand]
  (let [ hand-seq (seq hand)
	 x (map rank hand-seq)
	 freq (frequencies x)
	 values (vals freq)
	 filters (filter (fn [x] (= x 3)) values)]
	 (if (empty? filters)
		false
		true)))
	

(defn four-of-a-kind? [hand]
  (let [ hand-seq (seq hand)
	 x (map rank hand-seq)
	 freq (frequencies x)
	 values (vals freq)
	 filters (filter (fn [x] (= x 4)) values)]
	 (if (empty? filters)
		false
		true)))

(defn flush? [hand]
  (let [ hand-seq (seq hand)
	 x (map suit hand-seq)
	 freq (frequencies x)
	 values (vals freq)
	 filters (filter (fn [x] (= x 5)) values)]
	 (if (empty? filters)
		false
		true)))


(defn full-house? [hand]
	(and (three-of-a-kind? hand) (pair? hand)))
(defn two-pairs? [hand]
  (let [ hand-seq (seq hand)
	 x (map rank hand-seq)
	 freq (frequencies x)
	 values (vals freq)
	 filters (filter (fn [x] (= x 2)) values)]
	(if (= 4 (apply max values))
		true
		(if (= 2 (count filters))
			true
			false))))


(defn straight? [hand]
  (let [ hand-seq (seq hand)
	 x (map rank hand-seq)
	 normal-range (range (apply min x) (+ (apply min x) 5))
	 low-range (range 1 6)
	 y (replace {14 1} x)
	 x-sort (sort x)
	 y-sort (sort y)]
	(if (= x-sort normal-range)
		true
		(if (= y-sort low-range)
			true
			false))))


(defn straight-flush? [hand]
	(and (straight? hand) (flush? hand)))

(defn high-card? [hand]
	true)
(defn value [hand]
	(let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
	(apply max (map second (filter (fn [x] ((first x) hand)) checkers) ))))



