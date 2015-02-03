(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card]
    (Integer/valueOf
     (str
      (if (Character/isDigit r)
        r
        (let [replacements {\T 10, \J 11, \Q 12, \K 13, \A 14}]
          (replacements r)))))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn pair? [hand]
  (== 2 (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (== 3 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (== 4 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (== 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (let [frekvenssit (vals (frequencies (map rank hand)))]
    (= [2 3] (sort frekvenssit))))

(defn two-pairs? [hand]
  (let [frekvenssit (vals (frequencies (map rank hand)))]
    (or
     (four-of-a-kind? hand)
     (= [1 2 2] (sort frekvenssit)))))

;if is probably unnecessary...
(defn straight? [hand]
  (let [ranks (sort (map rank hand))]
    (let [pienin (first ranks)]
      (or
       (= ranks (range pienin (+ pienin 5)))
       (if (== 14 (apply max ranks))
         (= (sort (replace {14 1} ranks)) (range 1 6))
         false)))))


(defn straight-flush? [hand]
  (and
   (straight? hand)
   (flush? hand)))

(defn high-card? [hand] true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
    (let [zek (fn [[matcher? value]] (if (matcher? hand) value 0))]
      (apply max (map zek checkers)))))
