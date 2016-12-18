(ns p-p-p-pokerface)

(def replacements 
  {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[rk _] card
    val (cond
        (Character/isDigit rk) (Integer/valueOf (str rk))
        :else (replacements rk))]
    val))

(defn suit [card]
  (let [[_ st] card]
    (str st)))

(defn pair? [hand]
  (= 2 (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (= 3 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (= 4 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (= 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (= 
    (seq [2 3]) 
    (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (=
   (seq [1 2 2])
   (sort (vals (frequencies (map rank hand))))))
   ; four-of a kind?

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
