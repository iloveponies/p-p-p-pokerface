(ns p-p-p-pokerface)

(def letter-value {\A 14, \K 13, \Q 12, \J 11, \T 10})

(defn rank [card]
  (let [[rnk _] card]
    (if (Character/isDigit rnk)
          (Integer/valueOf (str rnk)) 
          (letter-value rnk))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn n-of-a-kind [hand]
  (apply max (vals (frequencies (map rank hand)))))

(defn pair? [hand]
  (< 1 (n-of-a-kind hand)))

(defn three-of-a-kind? [hand]
  (< 2 (n-of-a-kind hand)))

(defn four-of-a-kind? [hand]
  (< 3 (n-of-a-kind hand)))

(defn flush? [hand]
  (= 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (and
    (= 2 (apply min (vals (frequencies (map rank hand)))))
    (= 3 (apply max (vals (frequencies (map rank hand)))))))

(defn two-pairs? [hand]
  (= 2 (apply min (vals (frequencies (rest (sort (vals (frequencies (map rank hand))))))))))

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
    (straight-flush? hand)    8
    (four-of-a-kind? hand)    7
    (full-house? hand)          6
    (flush? hand)                 5
    (straight? hand)             4
    (three-of-a-kind? hand)   3
    (two-pairs? hand)           2
    (pair? hand)                   1
    :else                               0
    ))
