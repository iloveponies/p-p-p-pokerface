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


(defn pair? [hand]
  nil)

(defn three-of-a-kind? [hand]
  nil)

(defn four-of-a-kind? [hand]
  nil)

(defn flush? [hand]
  nil)

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
