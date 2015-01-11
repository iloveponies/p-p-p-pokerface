(ns p-p-p-pokerface)

(defn rank [card]
  (let [rank (first card)
        ranks [nil \1 \2 \3 \4 \5 \6 \7 \8 \9 \T \J \Q \K \A]]
    (.indexOf ranks rank)))

(defn suit [card]
  (str (get card 1)))

(defn high-card? [hand]
  true)

(defn pair? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))
        sorted-freqs (sort freqs)]
    (= '(1 1 1 2) sorted-freqs)))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))
        sorted-freqs (sort freqs)]
    (= '(1 1 3) sorted-freqs)))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))
        sorted-freqs (sort freqs)]
    (= '(1 4) sorted-freqs)))

(defn flush? [hand]
  (let [suits (map suit hand)
        suit-freqs (vals (frequencies suits))]
    (= 5 (first suit-freqs))))

(defn full-house? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))
        sorted-freqs (sort freqs)]
    (= '(2 3) sorted-freqs)))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))
        sorted-freqs (sort freqs)]
    (= '(1 2 2) sorted-freqs)))

(defn straight? [hand]
  (let [ace-high (map rank hand)
        ace-low (replace {14 1} ace-high)
        ace-high-freqs (sort (vals (frequencies ace-high)))
        ace-low-freqs (sort (vals (frequencies ace-low)))]
    (or (and (= '(1 1 1 1 1) ace-high-freqs)
             (= 4 (count (range (apply min ace-high) (apply max ace-high)))))
        (and (= '(1 1 1 1 1) ace-low-freqs)
             (= 4 (count (range (apply min ace-low) (apply max ace-low))))))))

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
   (high-card? hand) 0))
