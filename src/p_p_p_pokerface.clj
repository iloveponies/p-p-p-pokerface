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
(defn same-type? [type hand count]
  (contains? (set (vals (frequencies (map type hand)))) count))

(defn pair? [hand]
  (same-type? rank hand 2))

(defn three-of-a-kind? [hand]
  (same-type? rank hand 3))

(defn four-of-a-kind? [hand]
  (same-type? rank hand 4))

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
