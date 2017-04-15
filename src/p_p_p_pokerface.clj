(ns p-p-p-pokerface)

(defn rank [card]
  (let [high-cards {\T 10,
                    \J 11,
                    \Q 12,
                    \K 13,
                    \A 14}
        rank (first card)]

    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (high-cards rank))))

(defn suit [card]
  (str (last card)))

(defn pair? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (contains? (set freqs) 2)))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (contains? (set freqs) 3)))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (contains? (set freqs) 4)))

(defn flush? [hand]
  (let [suits (map suit hand)
        different-suits (count (vals (frequencies suits)))]
    (== 1 different-suits)))

(defn full-house? [hand]
  (let [ranks (map rank hand)
        freqs (sort (vals (frequencies ranks)))]
    (= [2 3] freqs)))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        freqs (sort (vals (frequencies ranks)))]
    (or (= [1 2 2] freqs)
        (four-of-a-kind? hand))))

(defn straight? [hand]
  (let [ranks1 (sort (map rank hand))
        ranks2 (sort (replace {14 1} ranks1)) ; change 14 go 1 to account for ace double meaning
        range1 (range (first ranks1) (+ 1 (last ranks1)))
        range2 (range (first ranks2) (+ 1 (last ranks2)))]
    (or (= range1 ranks1)
        (= range2 ranks2))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (cond
    (straight-flush? hand)  8
    (four-of-a-kind? hand)  7
    (full-house? hand)      6
    (flush? hand)           5
    (straight? hand)        4
    (three-of-a-kind? hand) 3
    (two-pairs? hand)       2
    (pair? hand)            1
    :else                   0))
