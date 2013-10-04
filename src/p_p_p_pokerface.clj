(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank suite] card]
    (def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})
    (if (Character/isDigit rank) (Integer/valueOf (str rank))
        (replacements rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn ranks-in-hand [hand]
  (map rank hand))

(defn suits-in-hand [hand]
  (map suit hand))

(defn rank-frequencies [hand]
  (vals (frequencies (ranks-in-hand hand))))

(defn suit-frequencies [hand]
  (vals (frequencies (suits-in-hand hand))))

(defn max-of-sequence [sequence]
  (apply max sequence))

(defn min-of-sequence [sequence]
  (apply min sequence))

(defn many-a-kind? [n hand]
  (>= (max-of-sequence (rank-frequencies hand)) n))

(defn pair? [hand]
  (many-a-kind? 2 hand))

(defn three-of-a-kind? [hand]
  (many-a-kind? 3 hand))

(defn four-of-a-kind? [hand]
  (many-a-kind? 4 hand))

(defn flush? [hand]
  (== (max-of-sequence (suit-frequencies hand)) 5))

(defn full-house? [hand]
  (= (sort (rank-frequencies hand)) [2 3]))

(defn two-pairs? [hand]
  (or (= (sort (rank-frequencies hand)) [1 2 2])
      (four-of-a-kind? hand)))

(defn straight? [hand]
  (let [sorted-rank-hand (sort (ranks-in-hand hand))]
    (or (= sorted-rank-hand [2 3 4 5 14])
        (= sorted-rank-hand
           (range (min-of-sequence sorted-rank-hand) (+ (max-of-sequence sorted-rank-hand) 1))))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.

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
