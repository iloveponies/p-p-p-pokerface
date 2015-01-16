(ns p-p-p-pokerface)

(defn rank [card]
  (let [face-map {\A 14, \K 13, \Q 12, \J 11, \T 10}
        [rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (get face-map rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (== 2 (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (== 3 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (== 4 (apply max (vals (frequencies (map rank hand))))))

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
