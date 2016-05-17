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
  (set (vals (frequencies (map rank hand)))))

(defn suit-freq [hand]
  (set (vals (frequencies (map suit hand)))))

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
  (= 1 (count (suit-freq hand))))

;Ex7 Write the function (full-house? hand) that returns true if hand is a full house, and otherwise false.
(defn full-house? [hand]
  (and
    (pair? hand)
    (three-of-a-kind? hand)))

;Ex8 Write the function (two-pairs? hand) that return true if hand has two pairs, and otherwise false.
;Note that a four of a kind is also two pairs.
(defn two-pairs? [hand]
  nil)

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)


