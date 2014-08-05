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
(defn number-of-type [type hand]
                (vals (frequencies (map type hand))))
(defn same-type? [type hand count]
  (contains? (set (number-of-type type hand)) count))

(defn pair? [hand]
  (same-type? rank hand 2))

(defn three-of-a-kind? [hand]
  (same-type? rank hand 3))

(defn four-of-a-kind? [hand]
  (same-type? rank hand 4))

(defn flush? [hand]
  (same-type? suit hand 5))

(defn full-house? [hand]
  (and (pair? hand)
       (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  nil)

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
