(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _ ] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get {\T 10, \J 11, \Q 12, \K 13, \A 14} r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn pair? [hand]
  (== 2 (apply max (vals (frequencies (map (fn [x] (rank x)) hand))))))

(defn three-of-a-kind? [hand]
  (== 3 (apply max (vals (frequencies (map (fn [x] (rank x)) hand))))))

(defn four-of-a-kind? [hand]
  (== 4 (apply max (vals (frequencies (map (fn [x] (rank x)) hand))))))

(defn flush? [hand]
  (== 5 (apply max (vals (frequencies (map (fn [x] (suit x)) hand))))))

(defn full-house? [hand]
  (= [2 3] (sort (vals (frequencies (map (fn [x] (rank x)) hand))))))

(defn two-pairs? [hand]
  (or (= [1 2 2] (sort (vals (frequencies (map (fn [x] (rank x)) hand)))))
      (four-of-a-kind? hand)))

(defn straight? [hand]
  (let [sorted-hand (sort (map (fn [x] (rank x)) hand))
        min-value (apply min sorted-hand)]
    (or (= (range min-value (+ min-value 5)) sorted-hand)
        (= [2 3 4 5 14] sorted-hand))))

(defn straight-flush? [hand]
  (and (flush? hand)
       (straight? hand)))

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
