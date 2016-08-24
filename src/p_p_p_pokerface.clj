(ns p-p-p-pokerface)

(def char-rank-map {"T" 10, "J" 11, "Q" 12, "K" 13, "A" 14})

(defn rank [card]
  (let [[rank _] card
        rank-str (str rank)]
    (if (Character/isDigit rank)
      (Integer/valueOf rank-str)
      (get char-rank-map rank-str))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
   (and
      (= 2 (apply max freqs))
      (= 3 (count (filter #(= 1 %) freqs))))))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (and
      (= 3 (apply max freqs))
      (= 2 (count (filter #(= 1 %) freqs))))))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (= 4 (apply max freqs))))

(defn flush? [hand]
  (let [suits (map suit hand)
        freqs (vals (frequencies suits))]
    (= 5 (apply max freqs))))

(defn full-house? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
   (and
      (= 3 (apply max freqs))
      (= 2 (apply min freqs)))))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (= 2 (count (filter #(= 2 %) freqs)))))

(defn straight? [hand]
  (let [ranks (distinct (map rank hand))
        sorted-ranks (sort ranks)
        min-rank (apply min ranks)
        ranged-ranks (range min-rank (+ min-rank 5))]
    (or 
      (= sorted-ranks ranged-ranks)
      (= sorted-ranks '(2 3 4 5 14)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn helper-fn [checker-fn hand value-to-return]
  (if (checker-fn hand)
    value-to-return))

(defn high-card? [hand] true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        points (map #(helper-fn (first %) hand (last %)) checkers)]
    (apply max (remove nil? points))))

