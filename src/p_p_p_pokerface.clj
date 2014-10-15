(ns p-p-p-pokerface)

(def face-map {\A 14, \K 13, \Q 12, \J 11, \T 10})

(defn rank [card]
  (let [[value _] card]
    (if (Character/isDigit value)
          (Integer/valueOf (str value))
          (face-map value))))

(defn suit [card]
  (let [[_ st] card]
    (str st)))

(defn pair? [hand]
  (contains? (vals (frequencies (map rank hand)) 2)))

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
