(ns p-p-p-pokerface)

(defn rank [card]
  (let [[ r _ ] card
        rank-values { \T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit r)
          (Integer/valueOf (str r))
          (rank-values r))))

(defn suit [card]
  (let [[ _ s ] card]
    (str s)))

(defn pair? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (>= (apply max (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (= (sort (vals (frequencies (map rank hand)))) [2 3]))

(defn two-pairs? [hand]
  (>= (count (filter #(>= % 2) (vals (frequencies (map rank hand))))) 2))

(defn straight? [hand]
  (let [ranks-high-ace (sort (map rank hand))
        ranks-low-ace (sort (replace {14 1} ranks-high-ace))
        check-straight (fn [ranks]
                         (= (range (first ranks) (+ (first ranks) 5)) ranks))]
    (or (check-straight ranks-high-ace)
        (check-straight ranks-low-ace))))

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
        check #((first %) hand)]
    (apply max (map second (filter check checkers)))))