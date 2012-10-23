(ns p-p-p-pokerface)

(defn rank [card]
  (def letternums {\T 10, \J 11, \Q 12, \K 13, \A 14})
  (if (Character/isDigit (first card))
    (Integer/valueOf (str (first card)))
    (letternums (first card))))

(defn suit [card]
  (str (second card)))

(defn pair? [hand]
  (contains? (set (vals (frequencies (map rank hand))) )2))

(defn three-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand))) )3))

(defn four-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand))) )4))

(defn flush? [hand]
  (let [pienin (first (sort (map rank hand)))]
    (and (= (first (vals (frequencies (map suit hand)))) 5)
         (not (= (sort (map rank hand)) (range pienin (+ pienin 5)))))))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (or (four-of-a-kind? hand)
      (contains? (set (vals (frequencies (vals (frequencies (map rank hand))))))2)))

(defn straight? [hand]
  (let [[ranksort ranksort2]
        [(sort (map rank hand)) (sort (replace {14,1} (map rank hand)))]]
    (let [[pienin pienin2] [(first ranksort) (first ranksort2)]]
      (and (or (= ranksort (range pienin (+ pienin 5)))
               (= ranksort2 (range pienin2 (+ pienin2 5))))))))

(defn straight-flush? [hand]
  (and
    (straight? hand)
    (= (first (vals (frequencies (map suit hand)))) 5 )))

(defn high-card? [hand]
    true)

(defn value [hand]
  (let [checkers [high-card? pair? two-pairs? three-of-a-kind? straight?
                  flush? full-house? four-of-a-kind? straight-flush?]
       hand-has-value? (fn [v] ((get checkers v) hand))]
        
    (apply max (filter hand-has-value? (range 0 9)))
      ))
