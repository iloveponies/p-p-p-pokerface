(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank_char _] card
        nobility {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit rank_char)
      (Integer/valueOf (str rank_char))
      (get nobility  rank_char))))

(defn suit [card]
  (let [[_  suit_char] card]
    (str suit_char)))

(defn pair? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 2))

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
