(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank] card]
    (if (Character/isDigit rank)
        (Integer/valueOf (str rank))
        (get {\T 10 \J 11 \Q 12 \K 13 \A 14} rank))))

(defn suit [card]
  (let [[_ suit] card] (str suit)))

(defn pair? [hand]
  (if
    (< 1 (apply max (vals (frequencies (mapv rank hand)))))
    true
    false))

(defn three-of-a-kind? [hand]
  (if
    (== 3 (apply max (vals (frequencies (mapv rank hand)))))
    true
    false))

(defn four-of-a-kind? [hand]
  (if
    (== 4 (apply max (vals (frequencies (mapv rank hand)))))
    true
    false))

(defn flush? [hand]
  (if
    (== 5 (apply max (vals (frequencies (mapv suit hand)))))
    true
    false))

(defn full-house? [hand]
  (if
   (= [2 3] (sort (vals (frequencies (mapv rank hand)))))
   true
   false
   ))

(defn two-pairs? [hand]
  (cond
   (four-of-a-kind? hand) true
   (= [1 2 2] (sort (vals (frequencies (mapv rank hand))))) true
   :else false
   ))

(defn straight? [hand]
 (let [x (sort (mapv rank hand)) y (sort (replace {14 1} (mapv rank hand)))]
   (cond
    (= x (range (apply min x) (+ 1 (apply max x)))) true
    (= y (range (apply min y) (+ 1 (apply max y)))) true
    :else false)))

(defn straight-flush? [hand]
 (and (straight? hand) (flush? hand)))

(defn value [hand]
  (cond
   (straight-flush? hand) 8
   (four-of-a-kind? hand) 7
   (full-house? hand) 6
   (flush? hand) 5
   (straight? hand) 4
   (three-of-a-kind? hand) 3
   (two-pairs? hand) 2
   (pair? hand) 1
   :else 0
   ))
