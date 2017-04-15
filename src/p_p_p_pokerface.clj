(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card]
    (def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})
      (if (Character/isDigit rank)
        (Integer/valueOf (str rank))
        (replacements rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (contains? (set (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (= [2 3] (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (= [1 2 2] (sort (vals (frequencies (map rank hand))))))

(defn straight? [hand]
  (let [sorted-values (sort (map rank hand))
        min-value (apply min sorted-values)
        max-value (apply max sorted-values)]
    (if (and (== min-value 2) (== max-value 14))
      (= [2 3 4 5 14] sorted-values)
      (= (range min-value (+ min-value 5)) sorted-values))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true)

(defn hand-has-value? [hand value]
  (let [checkers [high-card? pair? two-pairs? three-of-a-kind? straight?
                flush? full-house? four-of-a-kind? straight-flush?]]
    ((get checkers value) hand)))

(defn value [hand]
  (cond
    (hand-has-value? hand 8) 8
    (hand-has-value? hand 7) 7
    (hand-has-value? hand 6) 6
    (hand-has-value? hand 5) 5
    (hand-has-value? hand 4) 4
    (hand-has-value? hand 3) 3
    (hand-has-value? hand 2) 2
    (hand-has-value? hand 1) 1
    (hand-has-value? hand 0) 0))
