(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card
        replacements {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (not (Character/isDigit rank))
      (replacements rank)
      (Integer/valueOf (str rank)))))

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
  (= [3 2] (vals (frequencies (map rank hand)))))

(defn two-pairs? [hand]
  (or (= [4 1] (vals (frequencies (map rank hand)))) 
      (= [2 2 1] (vals (frequencies (map rank hand))))
      (full-house? hand)))

(defn straight? [hand]
  (let [new-hand-high (replace {1 14} (map rank hand))
        new-hand-low (replace {14 1} (map rank hand))
        sorted-hand-high (sort new-hand-high)
        sorted-hand-low (sort new-hand-low)
        first-card-high (first sorted-hand-high)
        last-card-high (last sorted-hand-high)
        first-card-low (first sorted-hand-low)
        last-card-low (last sorted-hand-low)]
    (or (= (range first-card-high (+ 1 last-card-high)) sorted-hand-high)
        (= (range first-card-low (+ 1 last-card-low)) sorted-hand-low))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  nil)
