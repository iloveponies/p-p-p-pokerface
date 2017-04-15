(ns p-p-p-pokerface)

(defn rank [[r s]]
  (def rep {\T 10, \J 11, \Q 12, \K 13, \A 14})
  (if (Character/isDigit r) (Integer/valueOf (str r)) (rep r)))

(defn suit [[r s]]
  (str s))

;; helper function
(defn ranks [hand] (map rank hand))

;; Ridiculously named helper.
;; Returns frequencies of ranks in hand
(defn ranks-frequencies [hand]
  (vals (frequencies (ranks hand))))

;; Ridiculously named helper.
;; Returns true or false if hand is composed of given composition of ranks i.e. full house [3 2]
(defn ranks-composition? [hand, rank-composition]
  (= rank-composition (ranks-frequencies hand)))

;; Ridiculously named helper.
;; Returns true or false if some rank is found more than given times from the hand i.e. rank 2 is found more than two times from hand {"2H" "2S" "2C" "5S" "AD"}
(defn rank-occurs-more-often-than? [hand, count]
  (> (apply max (ranks-frequencies hand)) count))

(defn pair? [hand]
  (rank-occurs-more-often-than? hand 1))

(defn three-of-a-kind? [hand]
  (rank-occurs-more-often-than? hand 2))

(defn four-of-a-kind? [hand]
  (rank-occurs-more-often-than? hand 3))

(defn flush? [hand]
  (let [suit-occurs-more-often-than? (fn [count]
          (> (apply max (vals (frequencies (map suit hand)))) count))]
  (suit-occurs-more-often-than? 4)))

(defn full-house? [hand]
  (ranks-composition? hand [3 2]))

(defn two-pairs? [hand]
  (ranks-composition? hand [2 2 1]))

(defn straight? [hand]
  (let [straight-ranks? (fn [ranks]
                          (def min-rank (apply min ranks))
                          (def straight-range(range min-rank (+ min-rank 5)))
                          (= ranks straight-range))]
    (def hand-ranks (ranks hand))
    (or (straight-ranks? (sort hand-ranks)) (straight-ranks? (sort (replace {14 1} hand-ranks))))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn value [hand]
  (cond
    (straight-flush? hand) 8
    (four-of-a-kind? hand) 	7
    (full-house? hand) 6
    (flush? hand) 5
    (straight? hand) 4
    (three-of-a-kind? hand) 3
    (two-pairs? hand) 2
    (pair? hand) 1
    :else 0
    ))
