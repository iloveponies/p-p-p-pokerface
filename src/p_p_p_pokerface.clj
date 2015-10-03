(ns p-p-p-pokerface)

(def rank-placeholder
  {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[rank' _] card]
    (if (Character/isDigit rank')
      (Integer/valueOf (str rank'))
      (Integer/valueOf
       (str (rank-placeholder rank'))))))

(defn suit [card]
  (let [[_ suit'] card]
    (str suit')))

(defn pair? [hand]
  (let [values-hand (vals (frequencies (map rank hand)))]
    (and
     (= 2 (apply max values-hand))
     (= 4 (count values-hand)))))

(defn three-of-a-kind? [hand]
  (= 3 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (= 4 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (let [full-house-format [2 3]]
    (=
     full-house-format
     (sort (vals (frequencies (map rank hand)))))))

(defn two-pairs? [hand]
  (let [vals-in-hand (vals (frequencies (map rank hand)))]
    (or
     (= [1 2 2] (sort vals-in-hand))
     (= 4 (apply max vals-in-hand)))))

(defn straight? [hand]
  (let [sorted-hand (sort (map rank hand))
        alias-to-last-in-rank {14 1}
        sorted-hand-with-alias (sort (replace alias-to-last-in-rank sorted-hand))
        diference-between-edges (fn [a-seq] (- (last a-seq) (first a-seq)))]
  (and
   (= 5 (count (keys (frequencies sorted-hand))))
   (or
    (= 4 (diference-between-edges sorted-hand))
    (= 4 (diference-between-edges sorted-hand-with-alias))))))

(defn straight-flush? [hand]
  (and
   (straight? hand)
   (flush? hand)))

(defn high-card? [hand] true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        is-match (fn [[fn' value]] [(fn' hand) value])]
  (apply max (map
              second
              (filter
               (fn [[is-applied value]] is-applied)
               (map is-match checkers))))))
