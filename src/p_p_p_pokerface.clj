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
  (= [2 3] (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (or
   (four-of-a-kind? hand)
   (= [1 2 2] (sort (vals (frequencies (map rank hand)))))))

(defn straight? [hand]
  (let [hand-rank (map rank hand)
        sorted-rank-high (sort hand-rank)
        sorted-rank-low (sort (replace {14 1} hand-rank))
        low-card-rank-high (first sorted-rank-high)
        low-card-rank-low (first sorted-rank-low)]
    (or
     (= (range low-card-rank-high (+ low-card-rank-high 5)) sorted-rank-high)
     (= (range low-card-rank-low (+ low-card-rank-low 5)) sorted-rank-low))))

(defn straight-flush? [hand]
  (and
   (straight? hand)
   (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [match? (fn [[func _]] (func hand))
        checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
    (apply max (map second (filter match? checkers)))))
