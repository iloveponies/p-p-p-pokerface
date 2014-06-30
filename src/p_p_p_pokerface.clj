(ns p-p-p-pokerface)

(def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (replacements rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn hand-frequencies [hand attribute]
  (vals (frequencies (map attribute hand))))

(defn highest-count [hand attribute]
  (apply max (hand-frequencies hand attribute)))

(defn high-card? [hand]
  true)

(defn pair? [hand]
  (> (highest-count hand rank) 1))

(defn three-of-a-kind? [hand]
  (> (highest-count hand rank) 2))

(defn four-of-a-kind? [hand]
  (> (highest-count hand rank) 3))

(defn flush? [hand]
  (= (highest-count hand suit) 5))

(defn full-house? [hand]
  (= (sort (hand-frequencies hand rank)) '(2 3)))

(defn two-pairs? [hand]
  (or (four-of-a-kind? hand)
      (full-house? hand)
      (= (sort (hand-frequencies hand rank)) '(1 2 2))))

(defn ranks [hand]
  (map rank hand))

(defn lowest-rank [ranks]
  (apply min ranks))

(defn straight-from [lowest-rank]
  (range lowest-rank (+ 5 lowest-rank)))

(defn straight? [hand]
  (let [ranks-with-high-ace (ranks hand)
        ranks-with-low-ace (replace {14 1} (ranks hand))]
    (boolean (some #(= (sort %) (straight-from (lowest-rank %)))
                   [ranks-with-high-ace ranks-with-low-ace]))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        matching-checkers (filter #((first %) hand) checkers)
        matching-scores (map second matching-checkers)]
    (apply max matching-scores)))
