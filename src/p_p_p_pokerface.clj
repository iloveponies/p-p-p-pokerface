(ns p-p-p-pokerface)

(defn high-rank [rk]
  (get {\T 10
        \J 11
        \Q 12
        \K 13
        \A 14} rk))

(defn rank [card]
  (let [[rk _] card]
    (if (Character/isDigit rk)
      (Integer/valueOf (str rk))
      (high-rank rk))))

(defn suit [card]
  (let [[_ st] card]
    (str st)))

(defn x-of-a-kind? [hand, x]
  (contains? (set (vals (frequencies(map rank hand)))) x))

(defn pair? [hand]
  (x-of-a-kind? hand 2))

(defn three-of-a-kind? [hand]
  (x-of-a-kind? hand 3))

(defn four-of-a-kind? [hand]
  (x-of-a-kind? hand 4))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [freqs (frequencies (vals (frequencies(map rank hand))))]
    (boolean (and (get freqs 2) (= 2 (get freqs 2))))))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        lowAceRanks (sort (replace {14 1} ranks))]
    (or (and (apply < ranks) (= (apply max ranks) (+ 4 (apply min ranks))))
        (and (apply < lowAceRanks) (= (apply max lowAceRanks) (+ 4 (apply min lowAceRanks)))))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
    (apply max (map (fn[x] (let [[_ v] x] v)) (filter (fn[x] ((get x 0) hand)) checkers)))))
