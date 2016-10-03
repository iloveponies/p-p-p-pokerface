(ns p-p-p-pokerface)

(def card-rank-values {\T 10
                       \J 11
                       \Q 12
                       \K 13
                       \A 14})



(defn rank
  "takes a single card and returns the rank as a number between 2 and 14."
  [card]
  (let [[rank _] card]
    (if (Character/isDigit rank)
     (Integer/valueOf (str rank))
     (card-rank-values rank)
     )))

(defn suit
  "takes a singe card and returns the suit of the card as a one character string."
  [card]
  (let [[_ suit] card]
    (str suit)))

(defn frequencies-in-hand
  [hand]
  (vals (frequencies (map rank hand)))
  )

(defn pair?
  "returns true if there is a pair in hand and false if there is no pair in hand."
  [hand]
  (contains? (set (frequencies-in-hand hand)) 2))

(defn max-pair?
  "returns true if there is maximum a pair in hand and false if there is no pair in hand."
  [hand]
  (= (apply max (frequencies-in-hand hand)) 2)
  )

(defn three-of-a-kind? [hand]
  (contains? (set (frequencies-in-hand hand)) 3))

(defn max-three-of-a-kind?
  "returns true if there is maximum a pair in hand and false if there is no pair in hand."
  [hand]
  (= (apply max (frequencies-in-hand hand)) 3)
  )

(defn four-of-a-kind? [hand]
  (contains? (set (frequencies-in-hand hand)) 4))

;;(defn flush? [hand]
;;  (= (count (vals (frequencies (map suit hand)))) 1))

 (defn flush?
   "returns true if the hand is a flush."
   [hand]
   (= (count (set (map suit hand))) 1))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

;;(defn two-pairs? [hand]
;;  (or
;;   (four-of-a-kind? hand)
;;   (= [1 2 2] (sort (frequencies-in-hand hand)))))

 (defn two-pairs?
   "return true if hand has two pairs, and otherwise false."
   [hand]
   (let [sorted-frequencies (sort (frequencies-in-hand hand))]
    (or
     (= [1 2 2] sorted-frequencies)
     (= [1 4] sorted-frequencies))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        low-card (apply min ranks)
        ranks-with-ace-adjusted (if (= low-card 2)
                                  (replace {14 1} ranks)
                                  ranks)
        sorted-ranks (sort ranks-with-ace-adjusted)]
    (= sorted-ranks (range (apply min ranks-with-ace-adjusted) (+ (apply min ranks-with-ace-adjusted) 5)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))


;Hand	Value
;High card (nothing)	0
;Pair	1
;Two pairs	2
;Three of a kind	3
;Straight	4
;Flush	5
;Full house	6
;Four of a kind	7
;Straight flush	8

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn value
  "returns the value of a hand according to the table above."
  [hand]
  (let [checker #{[high-card? 0] [pair? 1]
                  [two-pairs? 2] [three-of-a-kind? 3]
                  [straight? 4] [flush? 5]
                  [full-house? 6] [four-of-a-kind? 7]
                  [straight-flush? 8]}
        valid-hand? (fn [[function val]]
                      (function hand))
        filtered-combinations (filter valid-hand? checker)
        combination-value (map second filtered-combinations)]
    (apply max combination-value)))
