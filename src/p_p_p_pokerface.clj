(ns p-p-p-pokerface)

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn rank [card]
  (let [[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (get {\A 14, \K 13, \Q 12, \J 11, \T 10} rank))))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn pair? [hand]
  (= (apply max (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (= (apply max (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (= (apply max (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (= (apply max (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (let [occurrences (sort (vals (frequencies (map rank hand))))]
    (and (= (first occurrences) 2) (= (second occurrences) 3))))

(defn two-pairs? [hand]
  (let [occurrences (sort > (vals (frequencies (map rank hand))))]
    (and (= (first occurrences) 2) (= (second occurrences) 2))))

(defn straight? [hand]
  (let [ace-as-14 (sort (map rank hand))
        ace-as-1 (sort (replace {14 1} (map rank hand)))]
    (or
      (= ace-as-14 (range (first ace-as-14) (+ (last ace-as-14) 1)))
      (= ace-as-1 (range (first ace-as-1) (+ (last ace-as-1) 1))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        results (filter (fn [checker]
                          ((first checker) hand))
                        checkers)]
    (apply max (map second results))))
