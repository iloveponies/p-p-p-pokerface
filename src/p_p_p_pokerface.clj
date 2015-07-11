(ns p-p-p-pokerface)

(defn rank [[rank suit]]
  (let [mapping {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (mapping rank))))

(defn suit [[rank suit]]
  (str suit))

(defn ranks [hand]
  (map rank hand))

(defn suits [hand]
  (map suit hand))

(defn rank-counts [hand]
  (sort (vals (frequencies (ranks hand)))))

(defn n-of-a-kind [hand n]
  (contains? (set (rank-counts hand)) n))

(defn pair? [hand]
  (n-of-a-kind hand 2))

(defn three-of-a-kind? [hand]
  (n-of-a-kind hand 3))

(defn four-of-a-kind? [hand]
  (n-of-a-kind hand 4))

(defn flush? [hand]
  (let [suit-counts (sort (vals (frequencies (suits hand))))]
    (== (first suit-counts)
        5)))

(defn full-house? [hand]
  (= (rank-counts hand)
     '(2 3)))

(defn two-pairs? [hand]
  (or (= (rank-counts hand)
         '(1 2 2))
      (four-of-a-kind? hand)
      (full-house? hand)))

(defn straight? [hand]
  (let [ranks1 (sort (ranks hand))
        ranks2 (sort (replace {14 1} ranks1))
        rank-range (fn [x]
                     (let [min-rank (apply min x)
                           max-rank (apply max x)]
                       (range min-rank (+ max-rank 1))))]
    (and (= (rank-counts hand)
            '(1 1 1 1 1))
         (or (== (count (rank-range ranks1)) 5)
             (== (count (rank-range ranks2)) 5)))))

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
        checks? (fn [[test? _]] (test? hand))
        matches (map second (filter checks? checkers))
        best-match (apply max matches)]
    best-match))


