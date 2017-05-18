(ns p-p-p-pokerface)

(defn
  rank
  "Returns the rank of the card."
  [card]
  (let [[^char rank _] card
        digit? (Character/isDigit rank)
        values {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if digit?
      (Integer/valueOf (str rank))
      (values rank))))

(defn
  suit
  "Returns the suit of the card."
  [card]
  (let [[_ suit] card]
    (str suit)))

(defn
  amount-of-pairs
  "Returns the amount of pairs in the hand"
  [hand]
  (let [ranks (frequencies (map rank hand))]
    (count (filter #{2} (vals ranks)))))

(defn pair?
  "Returns true if the hand includes a pair"
  [hand]
  (< 0 (amount-of-pairs hand)))

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
