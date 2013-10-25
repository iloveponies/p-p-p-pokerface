(ns p-p-p-pokerface)

(defn rank [card]
  (let [rank-values {\T 10, \J 11, \Q 12, \K 13, \A 14}
        [rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (Integer/valueOf (str (rank-values rank))))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
 (let [freqs (frequencies (map rank hand))]
    (not (empty? (filter (fn [x] (= x 2)) (vals freqs))))))

(defn three-of-a-kind? [hand]
  (let [freqs (frequencies (map rank hand))]
    (not (empty? (filter (fn [x] (= x 3)) (vals freqs))))))

(defn four-of-a-kind? [hand]
  (let [freqs (frequencies (map rank hand))]
    (not (empty? (filter (fn [x] (= x 4)) (vals freqs))))))

(defn flush? [hand]
  (let [suites (map suit hand)]
    (= 1 (count (frequencies suites)))))

(defn full-house? [hand]
  (let [freqs (frequencies (map rank hand))
        sorted_frequency_values (sort (vals freqs))]
    (= [2 3] sorted_frequency_values)))

(defn two-pairs? [hand]
  (let [sorted_freqs (sort (vals (frequencies (map rank hand))))
        two (filter (fn [x] (= x 2)) sorted_freqs)]
    (or (four-of-a-kind? hand) (= 2 (count two)))))

(defn straight? [hand]
  (let [sorted_ranks (sort (map rank hand))
        replaced_ace (sort (replace {14 1} sorted_ranks))]
    (or
     (= replaced_ace (range (apply min replaced_ace) (+ 1 (apply max replaced_ace))))
     (= sorted_ranks (range (apply min sorted_ranks) (+ 1 (apply max sorted_ranks)))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value_asd [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
    ))

(defn value [hand]
  (cond
   (straight-flush? hand) 8
   (four-of-a-kind? hand) 7
   (full-house? hand) 6
   (flush? hand) 5
   (straight? hand) 4
   (three-of-a-kind? hand) 3
   (two-pairs? hand) 2
   (pair? hand) 1
   :else 0))
