(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank-character _] card
        ten-or-greater {\T 10, \J 11, \Q 12, \K 13, \A 14}]
  (if (Character/isDigit rank-character)
    (Integer/valueOf (str rank-character))
    (get ten-or-greater rank-character))))

(defn suit [card]
  (let [[_ suit-character] card]
    (str suit-character)))

(defn n-of-a-kind? [n hand]
  (let [ranks (map rank hand)
        counts-of-each-rank (vals (frequencies ranks))
        max-count-of-single-rank (apply max counts-of-each-rank)]
    (== n max-count-of-single-rank)))

(defn pair? [hand]
  (n-of-a-kind? 2 hand))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? 3 hand))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? 4 hand))

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

