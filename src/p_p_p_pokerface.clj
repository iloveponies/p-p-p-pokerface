(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r] card values {\T 10, \J 11, \Q 12, \K 13, \A 14}  ]
	(if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get values r)
     )))

(defn suit [card]
  (let [[_ s] card]
	(str s)
    ))

(defn pair? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 2))

(defn two-pairs? [hand]
  (or
   (= 2 (get (frequencies (vals (frequencies (map rank hand)))) 2))
   (= 1 (get (frequencies (vals (frequencies (map rank hand)))) 4))
   ))

(defn three-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 4))

(defn straight? [hand]
  (let [sorted (sort (map rank hand)) smallest (first sorted)
        alt (sort (replace {14 1} sorted)) alt-smallest (first alt)]
    (or
     (= sorted (range smallest (+ smallest 5)))
     (= alt (range alt-smallest (+ alt-smallest 5)))
     )))

(defn flush? [hand]
  (= (apply max (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (let [freqs (set (vals (frequencies (map rank hand))))]
	(and (contains? freqs 3) (contains? freqs 2))))

(defn straight-flush? [hand]
  (and
   (straight? hand)
   (flush? hand)
   ))

(defn value [hand]
  (let [high-card? (fn [hand] true)
        checkers [high-card? pair? two-pairs? three-of-a-kind? straight?
                flush? full-house? four-of-a-kind? straight-flush?]]
    (apply max (filter (fn [i] ((get checkers i) hand)) (range 0 9)))

  ))