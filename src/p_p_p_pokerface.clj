(ns p-p-p-pokerface)

(defn rank [card]
  (let [val (first card)]
    (if (Character/isDigit val)
      (Integer/valueOf (str val))
      ({\T 10 \J 11 \Q 12 \K 13 \A 14} val))))

(defn suit [card]
  (str(second card)))

(defn pair? [hand]
  (= ((frequencies (vals (frequencies (map rank hand)))) 2) 1))

(defn three-of-a-kind? [hand]
  (= ((frequencies (vals (frequencies (map rank hand)))) 3) 1))

(defn four-of-a-kind? [hand]
  (= ((frequencies (vals (frequencies (map rank hand)))) 4) 1))

(defn flush? [hand]
  (= (count (frequencies (map suit hand))) 1))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (= ((frequencies (vals (frequencies (map rank hand)))) 2) 2))

(defn straight? [hand]
  (let [rks  (sort (map rank hand))
        lt (apply min rks)
        rt (apply max rks)]
    (cond
      (and (= rt (+ 4 lt)) (apply < rks)) true
      (= rks (conj (vec (range 2 6)) 14)) true
      :else false)))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand] true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
    (apply max (map second (filter #((first %) hand) checkers) ))))

