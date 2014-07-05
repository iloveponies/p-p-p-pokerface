(ns p-p-p-pokerface)

(def rank-map {\T 10 \J 11 \Q 12 \K 13 \A 14})

(defn rank [[card-rank card-suit]]
  (cond
    (Character/isDigit card-rank) (Integer/valueOf (str card-rank))
    :else (get rank-map card-rank)))
    

(defn suit [[card-rank card-suit]]
  (str card-suit))

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
