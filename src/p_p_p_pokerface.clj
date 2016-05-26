(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rnk _] card
        values {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit rnk)
      (Integer/valueOf (str rnk))
      (get values rnk))))

(defn suit [card]
  (let [[_ st] card]
    (str st)))

(defn pair? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (= 1 (count (set (vals (frequencies (map suit hand)))))))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [freq-list (vals (frequencies (map rank hand)))]
    (or (= freq-list [2 2 1]) (= freq-list [4 1]))))

(defn straight? [hand]
    (let [high-ace-list (sort (map rank hand))
        low-ace-list (sort (replace {14 1} (map rank hand)))
        high-ace-lower (first high-ace-list)
        low-ace-lower (first low-ace-list)]
    (or
      (= (range high-ace-lower (+ high-ace-lower 5)) high-ace-list)
      (= (range low-ace-lower (+ low-ace-lower 5)) low-ace-list)
      )))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
    (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        matched (filter (fn [check] ((first check) hand)) checkers)
        values (map second matched)]
    (apply max values)))
