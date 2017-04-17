(ns p-p-p-pokerface)

(defn rank [card]
  (let [letters-ranks {\T 10
                      \J 11
                      \Q 12
                      \K 13
                      \A 14}
        [rank _] card]
    (cond
      (Character/isDigit rank) (Integer/valueOf (str rank))
      :else (get letters-ranks rank))))

(defn rank-ace-low [card]
  (let [letters-ranks {\T 10
                      \J 11
                      \Q 12
                      \K 13
                      \A 1}
        [rank _] card]
    (cond
      (Character/isDigit rank) (Integer/valueOf (str rank))
      :else (get letters-ranks rank))))


(defn suit [card]
  (let [[_ rank] card]
    (str rank)))

(defn many-a-kind? [n hand]
  (contains? (set (vals (frequencies (map rank hand)))) n))

(defn pair? [hand]
  (many-a-kind? 2 hand))

(defn three-of-a-kind? [hand]
  (many-a-kind? 3 hand))

(defn four-of-a-kind? [hand]
  (many-a-kind? 4 hand))

(defn flush? [hand]
  (contains? (set (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (and
    (many-a-kind? 3 hand)
    (many-a-kind? 2 hand)))

(defn two-pairs? [hand]
  (or
    ;has four of a kind
    (four-of-a-kind? hand)
    ;or frequencies sequence has 2 two times
    (= 2 (count (filter (fn [r] (= r 2)) (vals (frequencies (map rank hand))))))))

(defn straight? [hand]
  (let [ranks-ace-high (sort (map rank hand))
        lowest-ace-high (first ranks-ace-high)
        highest-ace-high (last ranks-ace-high)
        ranks-ace-low (sort (map rank-ace-low hand))
        lowest-ace-low (first ranks-ace-low)
        highest-ace-low (last ranks-ace-low)]
    (or
      (= ranks-ace-high (range lowest-ace-high (+ highest-ace-high 1)))
      (= ranks-ace-low (range lowest-ace-low (+ highest-ace-low 1))))))

(defn straight-flush? [hand]
  (and
    (straight? hand)
    (flush? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn hand-has-value? [hand value]
  (let [checkers [high-card? pair? two-pairs? three-of-a-kind? straight?
                  flush? full-house? four-of-a-kind? straight-flush?]]
    ((get checkers value) hand)
  ))

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
