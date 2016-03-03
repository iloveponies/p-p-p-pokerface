(ns p-p-p-pokerface)

(defn rank [card]
  (let [replacements
        {\A 14 \K 13 \Q 12 \J 11 \T 10}
        [rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (Integer/valueOf (str (replacements rank))))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (if (== 2 (apply max (vals (frequencies (map rank hand)))))
    true
    false))

(defn three-of-a-kind? [hand]
  (if (== 3 (apply max (vals (frequencies (map rank hand)))))
    true
    false))

(defn four-of-a-kind? [hand]
  (if (== 4 (apply max (vals (frequencies (map rank hand)))))
    true
    false))

(defn flush? [hand]
  (if (== 5 (apply max (vals (frequencies (map suit hand)))))
    true
    false))

(defn full-house? [hand]
  (if (= [2 3] (sort (vals (frequencies (map rank hand)))))
    true
    false))

(defn two-pairs? [hand]
  (let [sorted-freq (sort (vals (frequencies (map rank hand))))]
  (if (or (= [1 2 2] sorted-freq)
          (= [1 4] sorted-freq))
    true
    false)))

(defn straight? [hand]
  (let [sorted-ranks (sort (map rank hand))
        needed-range (range (apply min sorted-ranks) (+ 1 (apply max sorted-ranks)))]
    (cond
      (= needed-range sorted-ranks) true
      (= (range 1 6) (sort (replace {14 1} sorted-ranks))) true
      :else false)
    ))

(defn straight-flush? [hand]
  (if (and (straight? hand) (flush? hand)) true false))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card?  0]   [pair? 1]
                   [two-pairs?  2]   [three-of-a-kind? 3]
                   [straight?   4]   [flush? 5]
                   [full-house? 6]   [four-of-a-kind? 7]
                   [straight-flush? 8]}]

    (apply max (map (fn [x]
           (if ((first x) hand)
             (second x)
             0))
         checkers))))
