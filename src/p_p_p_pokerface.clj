(ns p-p-p-pokerface)

(defn rank [card]
   (let [[rank _] card]
     (if (Character/isDigit rank) (Integer/valueOf (str rank)) ({\T 10, \J 11, \Q 12, \K 13, \A 14} rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (> (apply max (vals (frequencies (map (fn [x] (rank x)) hand)))) 1))

(defn three-of-a-kind? [hand]
  (> (apply max (vals (frequencies (map (fn [x] (rank x)) hand)))) 2))

(defn four-of-a-kind? [hand]
  (> (apply max (vals (frequencies (map (fn [x] (rank x)) hand)))) 3))

(defn flush? [hand]
  (== (apply max (vals (frequencies (map (fn [x] (suit x)) hand)))) 5))

(defn full-house? [hand]
  (= [2 3] (sort (vals (frequencies (map (fn [x] (rank x)) hand))))))

(defn two-pairs? [hand]
     (or (= [2 3] (sort (vals (frequencies (map (fn [x] (rank x)) hand))))) (or (= [1 2 2] (sort (vals (frequencies (map (fn [x] (rank x)) hand))))) (= [1 4] (sort (vals (frequencies (map (fn [x] (rank x)) hand))))))))

(defn straight? [hand]
  (let [ranks (sort (map (fn [x] (rank x)) hand))
        [a b c d e] (sort (map (fn [x] (rank x)) hand))]
    (if (== e 14) (if (pair? hand) false (or (== (apply + ranks) 28) (== (apply + ranks) 60))) (if (pair? hand) false (== 4 (- e a)) ))))

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
  (apply max (map (fn [x] (second x)) (filter (fn [x] ((first x) hand) ) checkers)))))
