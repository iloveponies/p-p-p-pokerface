(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank-value] card
        face-card-values {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit rank-value)
      (Integer/valueOf (str rank-value))
      (get face-card-values rank-value))))

(defn suit [card]
  (let [[_ suit-name] card]
    (str suit-name)))

(defn most-matches [hand]
  (apply max (vals (frequencies (map rank hand)))))

(defn pair? [hand]
  (> (most-matches hand) 1))

(defn three-of-a-kind? [hand]
  (> (most-matches hand) 2))

(defn four-of-a-kind? [hand]
  (> (most-matches hand) 3))

(defn flush? [hand]
  (> (apply max (vals (frequencies (map suit hand)))) 4))

(defn full-house? [hand]
  (= (range 2 4) (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (or
    (four-of-a-kind? hand)
    (= (seq [1 2 2]) (sort (vals (frequencies (map rank hand)))))))

(defn straight? [hand]
  (let [incremental? (fn [xs] (apply = 1 (map - (rest xs) xs)))
        large (sort (map rank hand))
        small (sort (replace {14 1} large))]
    (or (incremental? large) (incremental? small))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1]
                   [two-pairs? 2] [three-of-a-kind? 3]
                   [straight? 4] [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        matching-hands (filter (fn [checker] (apply (first checker) [hand])) checkers)
        hand-values (map second matching-hands)]
    (apply max hand-values)))
