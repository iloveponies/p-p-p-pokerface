(ns p-p-p-pokerface)

(defn rank [[r _]]
  (if (Character/isDigit r)
    (Integer/valueOf (str r))
    ({\T 10, \J 11, \Q 12, \K 13, \A 14} r)))

(defn suit [[_ suit]]
  (str suit))

(defn ranks [hand]
  (map rank hand))

(defn suits [hand]
  (set (map suit hand)))

(defn rank-freqs [hand]
  (frequencies (vals (frequencies (ranks hand)))))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn pair? [hand]
  (= 1 (get (rank-freqs hand) 2)))

(defn three-of-a-kind? [hand]
  (= 1 (get (rank-freqs hand) 3)))

(defn four-of-a-kind? [hand]
  (= 1 (get (rank-freqs hand) 4)))

(defn flush? [hand]
  (= 1 (count (suits hand))))

(defn full-house? [hand]
  (and
   (= 1 (get (rank-freqs hand) 2))
   (= 1 (get (rank-freqs hand) 3))))

(defn two-pairs? [hand]
  (= 2 (get (rank-freqs hand) 2)))

(defn straight? [hand]
  (let [sorted (sort (ranks hand))]
    (and
     (= 5 (get (rank-freqs hand) 1))
     (or
      (= 4 (- (last sorted) (first sorted)))
      (and (= 14 (last sorted)) (= 5 (nth sorted 3)))))))

(defn straight-flush? [hand]
  (and
   (straight? hand)
   (flush? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (map (fn [x] (if ((first x) hand) (second x) (- 1))) checkers))))
