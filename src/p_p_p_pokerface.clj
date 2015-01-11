(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card
        replacements {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (not (Character/isDigit rank))
      (replacements rank)
      (Integer/valueOf (str rank)))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (< 1 (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (< 2 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (< 3 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (== 5 (apply max (vals (frequencies (map suit hand))))))

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
