(ns p-p-p-pokerface)

(defn rank [[first _]]
  (let [maph {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit first) (Integer/valueOf (str first)) (maph first))))

(defn suit [[_ second]]
  (str second))

(defn rank-distrib [hand]
  (vals (frequencies (map rank hand))))

(defn pair? [hand]
  (>= (apply max (rank-distrib hand)) 2))

(defn three-of-a-kind? [hand]
  (>= (apply max (rank-distrib hand)) 3))

(defn four-of-a-kind? [hand]
  (>= (apply max (rank-distrib hand)) 4))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (= (sort (rank-distrib hand)) [2 3]))

(defn two-pairs? [hand]
  (let [sort-seq (sort (rank-distrib hand))
        eq-sort (fn [seq] (= seq sort-seq))]
    (cond
      (eq-sort [1 2 2]) true
      (eq-sort [2 3]) true
      (eq-sort [1 4]) true
      :else false)))

(defn straight? [hand]
  (let [sort-hand (sort (map rank hand))
        min-sort-hand (first sort-hand)
        sort-hand-r (sort (replace {14 1} (map rank hand)))]
    (cond
      (= sort-hand (range min-sort-hand (+ 5 min-sort-hand) )) true
      (= sort-hand-r (range 1 6)) true
      :else false)))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
  (apply max (map (fn [[ff xx]] (if (ff hand) xx 0)) checkers))))
