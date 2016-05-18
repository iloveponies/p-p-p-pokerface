(ns p-p-p-pokerface)


;Ex1 Write the function (suit card) which takes a singe card and returns the suit of the card as a one character string.
(defn suit [card]
  (let [[_ snd] card] (str snd)))

;Ex2 Write the function (rank card) which takes a single card and returns the rank as a number between 2 and 14.
(defn rank [card]
  (let [[fst _] card
        replacements {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (replacements fst))))

(defn rank-freq [hand]
  (frequencies (vals (frequencies (map rank hand)))))

;Ex3 Write the function (pair? hand) that returns true if there is a pair in hand and false if there is no pair in hand.
(defn pair? [hand]
  (contains?
    (rank-freq hand)
    2))

;Ex4 Write the function (three-of-a-kind? hand) that returns true if the hand contains a three of a kind.
(defn three-of-a-kind? [hand]
  (contains?
    (rank-freq hand)
    3))


;Ex5 Write the function (four-of-a-kind? hand) that returns true if the hand contains a four of a kind.
(defn four-of-a-kind? [hand]
  (contains?
    (rank-freq hand)
    4))

;Ex6 Write the function (flush? hand) that returns true if the hand is a flush.
(defn flush? [hand]
  (let [suit-freq (set (vals (frequencies (map suit hand))))]
    (= 1 (count suit-freq))))

;Ex7 Write the function (full-house? hand) that returns true if hand is a full house, and otherwise false.
(defn full-house? [hand]
  (and
    (pair? hand)
    (three-of-a-kind? hand)))

;Ex8 Write the function (two-pairs? hand) that return true if hand has two pairs, and otherwise false.
;Note that a four of a kind is also two pairs.
(defn two-pairs? [hand]
  (or
    (= 2 ((rank-freq hand) 2))
    (four-of-a-kind? hand)))

;Ex9 Write the function (straight? hand) that returns true if hand is a straight, and otherwise false.
;Note that an ace is accepted both as a rank 1 and rank 14 card in straights.
(defn straight? [hand]
  (let [sorted-orig-ranks (sort (map rank hand))
        low-ace-ranks (replace {14 1} (map rank hand))
        sorted-low-ace-ranks (sort low-ace-ranks)
        first-rank (first (map rank hand))
        test-straight (range first-rank (+ first-rank 5))
        test-low-ace-straight (range 1 6)]
    (or
      (= sorted-orig-ranks test-straight)
      (= sorted-low-ace-ranks test-low-ace-straight))))

;Ex10 Write the function (straight-flush? hand) which returns true if the hand is a straight flush, that is both a straight and a flush, and otherwise false.
(defn straight-flush? [hand]
  (and
    (straight? hand)
    (flush? hand)))

;Ex11 Write the function (value hand), which returns the value of a hand
#_( Hand	              Value
    High card (nothing)	  0
    Pair	                1
    Two pairs	            2
    Three of a kind	      3
    Straight	            4
    Flush	                5
    Full house	          6
    Four of a kind	      7
    Straight flush	      8)

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]
                   [pair? 1]
                   [two-pairs? 2]
                   [three-of-a-kind? 3]
                   [straight? 4]
                   [flush? 5]
                   [full-house? 6]
                   [four-of-a-kind? 7]
                   [straight-flush? 8]}
        match-hands (filter  #((first %) hand)  checkers)
        match-values (map #(second %) match-hands)]
    (apply max match-values)
    )
  )


