(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card
        rank_lookup {\T 10,
                     \J 11,
                     \Q 12,
                     \K 13,
                     \A 14}]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get rank_lookup r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn pair? [hand]
  (= (apply max (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (= (apply max (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (= (apply max (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (= (apply max (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (let [freqs (vals (frequencies (map rank hand)))]
    (and
      (= (apply max freqs) 3)
      (= (apply min freqs) 2))))

(defn two-pairs? [hand]
  (let [freqs (vals (frequencies (map rank hand)))]
    (or (and
         (= (apply max freqs) 2)
         (= (count freqs) 3))
        (four-of-a-kind? hand))))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        is_straight_vals (fn [v] (= v (range (apply min v) (+ (apply min v) 5))))]
    (or
     (is_straight_vals ranks)
     (is_straight_vals (sort (replace {14 1} ranks))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

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
