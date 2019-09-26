(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank-string _] card
        char-ranks {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit rank-string) 
      (Integer/valueOf (str  rank-string))
      (char-ranks rank-string))
    ))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn number-of-n-of-a-kind [n hand]
  (let [counts (vals (frequencies (map rank hand)))]
    (count (filter #(= % n) counts))))

(defn n-of-a-kind? [n hand]
  (< 0 (number-of-n-of-a-kind n hand)))

(defn pair? [hand]
  (n-of-a-kind? 2 hand))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? 3 hand))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? 4 hand))

(defn flush? [hand]
  (= 1 (count (set (map suit hand)))))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (or (four-of-a-kind? hand)
      (= 2 (number-of-n-of-a-kind 2 hand))))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        ace1-ranks (sort (replace {14 1} ranks))
        straight-range (fn [first-rank] (range first-rank (+ first-rank 5)))
        is-straight? #(= % (straight-range (first  %)))]
    (or (is-straight? ranks) (is-straight? ace1-ranks))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [high-card? (fn [x] true)
        checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        passed-checkers (filter #((first %) hand) checkers)
        passed-checkers-values (map second passed-checkers)]
    (apply max passed-checkers-values)))
