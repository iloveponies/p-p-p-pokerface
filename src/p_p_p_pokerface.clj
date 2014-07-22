(ns p-p-p-pokerface)


;; takes a single card and returns the rank as a number between 2 and 14.

(defn rank [card]
  (let [[n] card
        replacements {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit n)
      (Integer/valueOf (str n))
      (replacements n))))


;; takes a singe card and returns the suit of the card as a one character string.

(defn suit [card]
  (let [[_ s] card]
    (str s)))


;; returns true if there is a pair in hand and false if there is no pair in hand.

(defn pair? [hand]
  (let [fq (frequencies (map rank hand))]
    (not (not-any? #(= % 2) (vals fq)))))


;; returns true if the hand contains a three of a kind.

(defn three-of-a-kind? [hand]
  (let [fq (frequencies (map rank hand))]
    (not (not-any? #(= % 3) (vals fq)))))


;;  returns true if the hand contains a four of a kind

(defn four-of-a-kind? [hand]
  (let [fq (frequencies (map rank hand))]
    (not (not-any? #(= % 4) (vals fq)))))


;;  that returns true if the hand is a flush.

(defn flush? [hand]
  (let [fq (frequencies (map suit hand))]
    (not (not-any? #(= % 5) (vals fq)))))


;; returns true if hand is a full house, and otherwise false

(defn full-house? [hand]
  (and (pair? hand)
       (three-of-a-kind? hand)))


;; return true if hand has two pairs, and otherwise false

(defn two-pairs? [hand]
  (let [fq (frequencies (map rank hand))
        fq2 (frequencies (vals fq))]
    (or (= 2 (get fq2 2))
        (= 1 (get fq2 4)))))


;; returns true if hand is a straight, and otherwise false

(defn straight? [hand]
  (let [fq (map rank hand)
        fq-set (into #{} fq)
        low-ace (and (contains? fq-set 2)
                     (contains? fq-set 14))
        result (sort (if low-ace
                       (replace {14 1} fq)
                       fq))
        first-el (first result)]
        (= result (range first-el (+ 5 first-el)))))


;; returns true if the hand is a straight flush, that is both a straight and a flush,
;; and otherwise false

(defn straight-flush? [hand]
  (and (flush? hand)
       (straight? hand)))


;; High card (nothing)	0
;; Pair	                1
;; Two pairs	          2
;; Three of a kind	    3
;; Straight	            4
;; Flush	              5
;; Full house	          6
;; Four of a kind	      7
;; Straight flush	      8

(defn value [hand]
  (cond
    (straight-flush? hand) 8
    (four-of-a-kind? hand) 7
    (full-house? hand) 6
    (flush? hand) 5
    (straight? hand) 4
    (three-of-a-kind? hand) 3
    (two-pairs? hand) 2
    (pair? hand) 1
    :else 0))
























