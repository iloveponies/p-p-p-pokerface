(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card]
  (if (Character/isDigit rank)
	(Integer/valueOf (str rank))
        ({\T 10 \J 11 \Q 12 \K 13 \A 14} rank))))

(defn suit [card]
  (let [[_ suit] card]
  (str suit)))

(defn pair? [hand]
  (if (contains? (set (vals (frequencies (map rank hand)))) 2)
	true
	false))

(defn three-of-a-kind? [hand]
  (if (contains? (set (vals (frequencies (map rank hand)))) 3)
	true
	false))

(defn four-of-a-kind? [hand]
  (if (contains? (set (vals (frequencies (map rank hand)))) 4)
	true
	false))

(defn flush? [hand]
  (if (contains? (set (vals (frequencies (map suit hand)))) 5)
	true
	false))

(defn full-house? [hand]
  (and (pair? hand)
       (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (or (= [1 2 2] (sort (vals (frequencies (map rank hand)))) )
   (four-of-a-kind? hand)))

(defn straight? [hand]
  (let [assa1 (sort (replace {14 1} (map rank hand)))
	assa14 (sort (map rank hand))]
 (or (= (range (first assa1) (+ (first assa1) 5)) assa1)
 (= (range (first assa14) (+ (first assa14) 5)) assa14))))

(defn straight-flush? [hand]
  (and (straight? hand)
   (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
(let [checkers [high-card?
		pair?
		two-pairs?
		three-of-a-kind?
		straight?
		flush?
		full-house?
		four-of-a-kind?
		straight-flush?]
      hand-has-value? (fn [value] ((get checkers value) hand))]
(apply max (filter hand-has-value? (range 9)))))
