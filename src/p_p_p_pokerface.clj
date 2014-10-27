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
  (contains? (set (vals (frequencies(map rank hand))))2))

(defn three-of-a-kind? [hand]
  (contains? (set (vals (frequencies(map rank hand))))3))

(defn four-of-a-kind? [hand]
  (contains? (set (vals (frequencies(map rank hand))))4))

(defn flush? [hand]
  (= 1 (count (distinct(map suit hand)))))
  

(defn full-house? [hand]
  (and (pair? hand)  (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  nil)

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
