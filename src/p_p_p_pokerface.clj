(ns p-p-p-pokerface)

(defn rank [card]
  (let [high-cards {\T 10, \J 11, \Q 12, \K 13, \A 14}
        [rank- suit] card]
    (if (Character/isDigit rank-)
        (Integer/valueOf (str rank-))
        (get high-cards rank-))))

(defn suit [card]
  (let [[rank suit] card]
    (str suit)))

(defn high-card? [hand]
    true) ; All hands have a high card.

(defn pair? [hand]
  (not (empty? (filter (fn [x] (> x 1)) (vals (frequencies (map rank hand)))))))

(defn three-of-a-kind? [hand]
  (not (empty? (filter (fn [x] (> x 2)) (vals (frequencies (map rank hand)))))))

(defn four-of-a-kind? [hand]
  (not (empty? (filter (fn [x] (> x 3)) (vals (frequencies (map rank hand)))))))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (let [rank-freqs (set (vals (frequencies (map rank hand))))]
    (= (set [2 3]) rank-freqs)))

(defn two-pairs? [hand]
  (>= (apply + (filter (fn [x] (> x 1)) (vals (frequencies (map rank hand))))) 4))

(defn straight? [hand]
  (let [sorted-hand (sort (map rank hand))
        min-rank (first sorted-hand)
        straight-length 5]
    (if (and (= (last sorted-hand) 14) (= min-rank 2))
        (= (sort (replace {14 1} sorted-hand)) (range 1 (+ 1 straight-length)))
        (= sorted-hand (range min-rank (+ min-rank straight-length))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                                    [two-pairs? 2]  [three-of-a-kind? 3]
                                    [straight? 4]   [flush? 5]
                                    [full-house? 6] [four-of-a-kind? 7]
                                    [straight-flush? 8]}
        true-checks (filter (fn [checker] (let [fun (first checker)] (fun hand))) checkers)
        weights (map second true-checks)]
    (apply max weights)))
