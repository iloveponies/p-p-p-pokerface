(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      ({\T 10, \J 11, \Q 12, \K 13, \A 14} fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn high-card? [hand]
  true)

(defn pair? [hand]
  (= (apply max (vals (frequencies (map rank hand))))
     2))

(defn three-of-a-kind? [hand]
  (= (apply max (vals (frequencies (map rank hand))))
     3))

(defn four-of-a-kind? [hand]
 (= (apply max (vals (frequencies (map rank hand))))
    4))


(defn flush? [hand]
  (= (apply max (vals (frequencies(map suit hand))))
     5))

(defn full-house? [hand]
  (= [2 3] (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (let [freq (sort (vals (frequencies (map rank hand))))]
    (or (= [1 2 2] freq)
        (= [1 4] freq))))

(defn straight? [hand]
  (let [sorted-hand (sort (map rank hand))
        min-value (apply min sorted-hand)]
    (if (and (= 2 min-value)
             (= 14 (apply max sorted-hand)))
      (= (range 1 6) (sort (replace {14 1} sorted-hand)))
      (= (range min-value (+ 5 min-value)) sorted-hand))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
    (apply max (filter identity (map (fn [x]
           (if ((first x) hand)
             (second x))) checkers)))))
