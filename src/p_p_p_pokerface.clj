(ns p-p-p-pokerface)

(defn
  rank
  "Returns the rank of the card."
  [card]
  (let [[^char rank _] card
        digit? (Character/isDigit rank)
        values {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (str (if digit?
           (Integer/valueOf (str rank))
           (values rank)))))

(defn
  suit
  "Returns the suit of the card."
  [card]
  (let [[_ suit] card]
    (str suit)))

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
