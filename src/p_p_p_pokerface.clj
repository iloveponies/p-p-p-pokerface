(ns p-p-p-pokerface)

(def replacements {\T 10 \J 11 \Q 12 \K 13 \A 14})

(defn rank [card]
  (let [[r _] card]
	(if (Character/isDigit r)
	  (Integer/valueOf (str r))
	  (replacements r))))

(defn suit [card]
  (let [[_ s] card]
	(str s)))

(defn max-occurence [pos-fn hand]
  (apply max (vals (frequencies (map pos-fn hand)))))

(defn pair? [hand]
  (>= (max-occurence rank hand) 2))

(defn three-of-a-kind? [hand]
  (>= (max-occurence rank hand) 3))

(defn four-of-a-kind? [hand]
  (>= (max-occurence rank hand) 4))

(defn flush? [hand]
  (= (max-occurence suit hand) 5))

(defn full-house? [hand]
  (= (sort (vals (frequencies (map rank hand)))) '(2 3)))

(defn two-pairs? [hand]
  (or
   (full-house? hand)
   (four-of-a-kind? hand)
   (= (sort (vals (frequencies (map rank hand)))) '(1 2 2))))

(defn straight? [hand]
  (let [hand-vals (map rank hand)
		min-val (first hand-vals)]
	(or
	 (= (sort hand-vals) (range min-val (+ min-val 5)))
	 (= (sort (replace {14 1} hand-vals)) (range 1 6)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1]
				   [two-pairs? 2] [three-of-a-kind? 3]
				   [straight? 4] [flush? 5]
				   [full-house? 6] [four-of-a-kind? 7]
				   [straight-flush? 8]}]
	(apply max (map second (filter #((first %) hand) checkers)))))
