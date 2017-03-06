(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r s] card
        pmap {\A 14, \K 13, \Q 12, \J 11, \T 10}]
    (or (get pmap r) (and (Character/isDigit r) (Integer/valueOf (str r))))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn pair? [hand]
  (not (empty? (filter (fn [c] (= c 2)) (vals (frequencies (map rank hand)))))))

(defn three-of-a-kind? [hand]
  (not (empty? (filter (fn [c] (= c 3)) (vals (frequencies (map rank hand)))))))

(defn four-of-a-kind? [hand]
  (not (empty? (filter (fn [c] (= c 4)) (vals (frequencies (map rank hand)))))))

(defn flush? [hand]
  (= (count (set (map suit hand))) 1))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [[fst scnd] (sort hand)
        [_ _ thrd frth] (sort hand)]
    (and (= (rank fst) (rank scnd)) (= (rank thrd) (rank frth)))))

(defn straight? [hand]
  (or (= (range (first (sort (map rank hand))) (+ 1 (last (sort (map rank hand))))) (sort (map rank hand))) (= (range 1 (+ 1 (last (sort (replace {14 1} (map rank hand)))))) (sort (replace {14 1} (map rank hand))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1]
                   [two-pairs? 2] [three-of-a-kind? 3]
                   [straight? 4]  [flush? 5]
                   [full-house? 6][four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (map second (filter (fn [ch] (contains? (set (filter (fn [c] (c hand)) (map first checkers))) (first ch))) checkers)))))
