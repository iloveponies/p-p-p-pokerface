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


(defn full-house? [hand]
  nil)

(defn two-pairs? [hand]
  nil)

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
