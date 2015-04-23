(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (get {\T 10, \J 11, \Q 12, \K 13, \A 14} rank))))

(defn suit [card]
  (let [[_ suit] card]
  (str suit)))

(defn pair? [hand]
  (= (apply max (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
    (= (apply max (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
    (= (apply max (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (= (apply max (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (= (sort (vals (frequencies (map rank hand)))) [2 3]))

(defn two-pairs? [hand]
  (or (= (sort (vals (frequencies (map rank hand)))) [1 2 2])
      (= (sort (vals (frequencies (map rank hand)))) [1 4])))

(defn straight? [hand]
  (= (sort (keys (frequencies (map rank hand)))) (range (apply min (map rank hand)) (+ 1 (apply max (map rank hand)))))


(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
