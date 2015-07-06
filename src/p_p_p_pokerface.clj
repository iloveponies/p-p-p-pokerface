(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card
        rankvalues {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (rankvalues rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn high-card? [hand]
  true)

(defn pair? [hand]
  (not (empty? (filter (fn [x] (>= x 2)) (vals (frequencies (map rank hand)))))))

(defn three-of-a-kind? [hand]
  (not (empty? (filter (fn [x] (>= x 3)) (vals (frequencies (map rank hand)))))))

(defn four-of-a-kind? [hand]
  (not (empty? (filter (fn [x] (>= x 4)) (vals (frequencies (map rank hand)))))))

(defn flush? [hand]
  (apply = 5 (vals (frequencies (map suit hand)))))

(defn full-house? [hand]
  (= [2 3] (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (let [sortedvals (sort (vals (frequencies (map rank hand))))]
    (or (= [1 2 2] sortedvals) (= [1 4] sortedvals))))

(defn straight? [hand]
  (let [sorted-hand (sort (map rank hand))
        ace-1-hand  (sort (replace {14 1} (map rank hand)))
        first-card  (apply min sorted-hand)]
    (or (= sorted-hand (range first-card (+ 5 first-card)))
        (= ace-1-hand (range 1 6)))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (map second (filter (fn [x] ((first x) hand)) checkers)))))
