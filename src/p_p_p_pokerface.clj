(ns p-p-p-pokerface)

(def rank_map {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn suit [card]
  (let [[_ the-suit] card]
    (str the-suit)))

(defn rank [card]
  (let [[the-rank _] card]
    (if (Character/isDigit the-rank)
     (Integer/valueOf (str the-rank))
      (rank_map the-rank))))

(defn pair? [hand]
  (<= 2 (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (<= 3 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (<= 4 (apply max (vals (frequencies (map rank hand))))))


(defn flush? [hand]
  (= 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (= '(2 3) (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (let [sq (sort (vals (frequencies (map rank hand))))]
    (or (= sq '(1 2 2))
        (= sq '(1 4)))))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        mini1 (apply min ranks)
        ranks2 (sort (replace {14 1} ranks))
        mini2 (apply min ranks2)]
    (or (= ranks (range mini1 (+ mini1 5)))
        (= ranks2 (range mini2 (+ mini2 5))))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))
(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        aux (fn [[a b]] (if (a hand) b 0))]
    (apply max (map aux checkers))))
