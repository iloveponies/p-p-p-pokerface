(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (get {\T 10, \J 11, \Q 12, \K 13, \A 14} fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
   (= 2 (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (and
  (= 3 (apply max (vals (frequencies (map rank hand)))))
  (not (contains? (set (vals (frequencies (map rank hand)))) 2))))

(defn four-of-a-kind? [hand]
  (= 4 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (= 5 (first (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (and
   (contains? (set (vals (frequencies (map rank hand)))) 3)
   (contains? (set (vals (frequencies (map rank hand)))) 2)))

(defn two-pairs? [hand]
  (=
   (first (frequencies (vals (frequencies (map rank hand)))))
   [2 2]))

(defn straight? [hand]
  (cond
   (and
    (= 14 (apply max (map rank hand)))
    (= 10 (apply min (map rank hand))))
   (=
    (range 10 15)
    (sort (map rank hand)))
   (and
    (= 2 (apply min (map rank hand)))
    (= 14 (apply max (map rank hand))))
   (=
    [1, 2, 3, 4, 5]
    (sort (replace {14 1} (map rank hand))))
   :else
   (=
    (sort (map rank hand))
    (range (first (sort (map rank hand))) (+ 5 (first (sort (map rank hand))))))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))


(defn value [hand]
  nil)
