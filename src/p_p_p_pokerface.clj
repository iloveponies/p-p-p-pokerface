(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank suit] card]
    (if (Character/isDigit rank)
      (- (int rank) 48)
      (get {\T 10, \J 11, \Q 12,
            \K 13, \A 14} rank))))


(defn suit [card]
  (let [[rank suit] card]
    (str suit)))


(defn rank-counts [hand]
  (set (vals (frequencies (map rank hand)))))


(defn sorted-rank-counts [hand]
  (reverse (sort (vals (frequencies (map rank hand))))))


(defn suit-counts [hand]
  (set (vals (frequencies (map suit hand)))))


(defn pair? [hand]
  (contains? (rank-counts hand) 2))


(defn three-of-a-kind? [hand]
  (contains? (rank-counts hand) 3))


(defn four-of-a-kind? [hand]
  (contains? (rank-counts hand) 4))


(defn flush? [hand]
  (contains? (suit-counts hand) 5))


(defn full-house? [hand]
  (and
   (pair? hand)
   (three-of-a-kind? hand)))


(defn two-pairs? [hand]
  (or
   (and
    (>= (first (sorted-rank-counts hand)) 2)
    (== (second (sorted-rank-counts hand)) 2))
   (four-of-a-kind? hand)))


(defn straight? [hand]
    (or
     (let [sorted (sort (map rank hand))
           min-rank (apply min sorted)
           comp-seq (seq (range min-rank (+ min-rank 5)))]
       (= sorted comp-seq))
     (let [sorted (sort (replace {14 1} (map rank hand)))
           min-rank (apply min sorted)
           comp-seq (seq (range min-rank (+ min-rank 5)))]
       (= sorted comp-seq))))


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
    (apply max (map second (filter (fn [checker] ((first checker) hand)) checkers)))))

