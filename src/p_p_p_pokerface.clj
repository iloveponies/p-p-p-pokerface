(ns p-p-p-pokerface)

(defn rank [[R _]]
  (let [ranks {\T 10,\J 11,\Q 12,\K 13,\A 14}]
    (cond
     (Character/isDigit R) (Integer/valueOf (str R))
     :else (get ranks R))))

(defn suit [[_ S]]
  (str S))

(defn pair? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (== 1 (count (set (map suit hand)))))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (or 
   (== 2 (count (filter (fn [x] (== x 2)) (vals (frequencies (map rank hand))))))
   (four-of-a-kind? hand)))

(defn straight? [hand]
  (let [values1 (sort(map rank hand))
        values2 (sort (replace {14, 1} values1))]
    (or (= (range (first values1) (+ 5 (first values1))) values1)
        (= (range (first values2) (+ 5 (first values2))) values2))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

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
   :else 0))