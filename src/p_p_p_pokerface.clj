(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rnk _] card]
  (cond
   (Character/isDigit rnk) (Integer/valueOf (str rnk))
   :else ({\T 10, \J 11, \Q 12, \K 13, \A 14} rnk))))


(defn suit [card]
  (let [[_ snd] card]
  (str snd)))

(defn pair? [hand]
  (if (=
       (apply max (vals ( frequencies (map rank hand))))
          2) true false))

(defn three-of-a-kind? [hand]
   (if (=
       (apply max (vals ( frequencies (map rank hand))))
          3) true false))

(defn four-of-a-kind? [hand]
   (if (=
       (apply max (vals ( frequencies (map rank hand))))
          4) true false))

(defn flush? [hand]
    (if (=
       (apply max (vals ( frequencies (map suit hand))))
          5) true false))

(defn full-house? [hand]
  (let [[c1 c2 c3 c4 c5] hand]
      (sort hand)
         (cond
          ((pair?[c1 c2])(three-of-a-kind?[c3 c4 c5])) true
          ((pair?[c1 c2 c3])(three-of-a-kind?[c3 c4])) true
          :else (false))))



(defn two-pairs? [hand]
  nil)

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)


