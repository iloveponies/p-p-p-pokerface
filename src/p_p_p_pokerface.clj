(ns p-p-p-pokerface)




(defn rank [card]
  (let[[r _] card value{\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit r)
      (Integer/valueOf(str r))
      (get value r))))



(defn suit [card]
  (let[[_ s]card]
    (str s))
)


(defn pair? [hand]
  (== 2 (apply max(vals(frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (== 3 (apply max(vals(frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (== 4 (apply max(vals(frequencies (map rank hand))))))

(defn flush? [hand]
  (== 5 (apply max(vals(frequencies (map suit hand))))))

(defn full-house? [hand]
  (and
    (three-of-a-kind? hand)
    (== 2(apply min(vals(frequencies(map rank hand)))))))

(defn two-pairs? [hand]
  (and
    (not(three-of-a-kind? hand))
    (== 3 (count(set(keys(frequencies (map rank hand))))))))

(defn straight? [hand]
  (let [ace-high (sort (map rank hand)) ace-low (sort (replace {14 1} (map rank hand)))]
    (if (= ace-high (range (apply min ace-high) (+ (apply min ace-high) 5)))
    true
    (= ace-low (range (apply min ace-low) (+ (apply min ace-low) 5))))))

(defn straight-flush? [hand]
  (and
   (straight? hand)
   (flush? hand)))

(defn value [hand]
  (cond
    (straight-flush? hand)   8
    (four-of-a-kind? hand)   7
    (full-house? hand)       6
    (flush? hand)            5
    (straight? hand)         4
    (three-of-a-kind? hand)  3
    (two-pairs? hand)        2
    (pair? hand)             1
    :else                    0
    ))
