(ns p-p-p-pokerface)

(defn rank [card]
  (let [[value _] card]
  (if (Character/isDigit value)
  (Integer/valueOf (str value))
  ({\T 10 \J 11 \Q 12 \K 13 \A 14} value))))

(defn suit [card]
  (let [[_ color] card]
  (str color)))

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
