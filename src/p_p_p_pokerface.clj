(ns p-p-p-pokerface)


;; Ex 1
;; Write the function (suit card) which takes a singe card and returns the suit of the card as a one character string.

(defn suit [card]
  (let [ [_ suit] card]
  (str suit)))


;; Ex 2
;; Write the function (rank card) which takes a single card and returns the rank as a number between 2 and 14.

(defn rank [card]
  (let [ [rank _] card]
  ({\2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9 \T 10 \J 11 \Q 12 \K 13 \A 14} rank)))


;; Ex 3
;; Write the function (pair? hand) that returns true if there is a pair in hand and false if there is no pair in hand.

(defn pair? [hand]
  (not (apply distinct? (map rank hand))))


;; Ex 4
;; Write the function (three-of-a-kind? hand) that returns true if the hand contains a three of a kind.

(defn three-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 3))


;; Ex 5
;; Write the function (four-of-a-kind? hand) that returns true if the hand contains a four of a kind.

(defn four-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 4))


;; Ex 6
;; Write the function (flush? hand) that returns true if the hand is a flush.

(defn flush? [hand]
  (= 1 (count (set (map suit hand)))))


;; Ex 7
;; Write the function (full-house? hand) that returns true if hand is a full house, and otherwise false.

(defn full-house? [hand]
  (= #{3 2} (set (vals (frequencies (map rank hand))))))


;; Ex 8
;; Write the function (two-pairs? hand) that return true if hand has two pairs, and otherwise false.
;; Note that a four of a kind is also two pairs.

(defn two-pairs? [hand]
  (let [card-ranks               (map rank hand)
        card-rank-freqs          (frequencies card-ranks)
        card-rank-freq-vals      (vals card-rank-freqs)
        card-rank-freq-val-freqs (frequencies card-rank-freq-vals)]
    (or (= 1 (get card-rank-freq-val-freqs 4 ))
        (= 2 (get card-rank-freq-val-freqs 2 )))))


;; Ex 9
;; Write the function (straight? hand) that returns true if hand is a straight, and otherwise false.
;; Note that an ace is accepted both as a rank 1 and rank 14 card in straights.

(defn straight? [hand]
  (let [all-ranks (map rank hand)
        max-card (apply max all-ranks)
        min-card (apply min all-ranks)
        all-ranks-lo (map #(if (= 14 (rank %1)) 1 (rank %1)) hand)
        max-card-lo (apply max all-ranks-lo)
        min-card-lo (apply min all-ranks-lo)]
    (and
      (or (= 4 (- max-card min-card))
          (= 4 (- max-card-lo min-card-lo)))
      (= 5 (count (distinct all-ranks))))))


;; Ex 10
;; Write the function (straight-flush? hand) which returns true if the hand is a straight flush, that is both a straight and a flush, and otherwise false.

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))


;; Ex 11
;; Write the function (value hand), which returns the value of a hand according to the table above.

(defn value [hand]
  (cond (straight-flush? hand)  8
        (four-of-a-kind? hand)  7
        (full-house? hand)      6
        (flush? hand)           5
        (straight? hand)        4
        (three-of-a-kind? hand) 3
        (two-pairs? hand)       2
        (pair? hand)            1
        :else 0))


