(ns p-p-p-pokerface)

(defn rank [[r]]
  (if (Character/isDigit r)
    (Integer/valueOf (str r))
    (get {\T 10 \J 11 \Q 12 \K 13 \A 14} r)))

(defn suit [[_ s]]
  (str s))

(defn pair? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 2))

(defn two-pairs? [hand]
  nil)

(defn three-of-a-kind? [hand]
  nil)

(defn four-of-a-kind? [hand]
  nil)

(defn straight? [hand]
  nil)

(defn flush? [hand]
  nil)

(defn full-house? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
