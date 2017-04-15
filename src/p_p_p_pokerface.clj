(ns p-p-p-pokerface)

(defn rank [card]
  (def alphas {\A 14, \K 13, \Q 12, \J 11, \T 10})
  (let [[c _] card]
    (if (Character/isDigit c)
      (Integer/valueOf (str c))
        (alphas c))))


(defn suit [card]
 (let [[_ c] card]
  (str c)))

(defn pair? [hand]
  (< 1 (apply max (vals (frequencies (map rank hand))))))


(defn three-of-a-kind? [hand]
  (< 2 (apply max (vals (frequencies (map rank hand))))))


(defn four-of-a-kind? [hand]
  (< 3 (apply max (vals (frequencies (map rank hand))))))


(defn flush? [hand]
  (< 4 (apply max (vals (frequencies (map suit hand))))))


(defn full-house? [hand]
  (let [values (sort (vals (frequencies (map rank hand))))]
     (= values (range 2 4))))


(defn two-pairs? [hand]
 (let [values (sort (vals (frequencies (map rank hand))))]
     (or (= (rest values) [2 2]) (= (rest values) [4]))))


(defn straight? [hand]
  (let [val1 (sort (map rank hand))
        val2 (sort (replace {14 1} (map rank hand)))
        ]
    (or
     (= val1 (range (first val1) (+ (first val1) 5)))
     (= val2 (range (first val2) (+ (first val2) 5))))))


(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))


(defn value [hand]
  (cond
    (straight-flush? hand) 8
    (four-of-a-kind? hand) 7
    (full-house? hand) 6
    (flush? hand)  5
    (straight? hand) 4
    (three-of-a-kind? hand) 3
    (two-pairs? hand) 2
    (pair? hand) 1
    :else 0))

