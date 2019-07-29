(ns p-p-p-pokerface)

(defn rank [card]
  (let [rankChr (first card)]
    (if (Character/isDigit rankChr)
      (Integer/valueOf (str rankChr))
      (let [rankConv {\T 10 \J 11 \Q 12 \K 13 \A 14}]
        (rankConv rankChr)))))

(defn suit [card]
  (str (second card)))

(defn rank-freqs [hand]
  (vals (frequencies (map rank hand))))

(defn n-of-a-kind? [n hand]
  (let [rf (rank-freqs hand)]
    (= n (apply max rf))))

(defn pair? [hand]
  (n-of-a-kind? 2 hand))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? 3 hand))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? 4 hand))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (let [rf (rank-freqs hand)]
    (and (= 3 (apply max rf)) (= 2 (count rf)))))

(defn two-pairs? [hand]
  (let [rf (rank-freqs hand)]
    (and (= 2 (apply max rf)) (= 3 (count rf)))))

(defn straight-by-ranks? [ranks]  
  (let [sorted-ranks (sort ranks)
        smallest (first sorted-ranks)
        straight (range smallest (+ 5 smallest))]
    (= straight sorted-ranks)))

(defn straight? [hand]
  (let [ranks (map rank hand)
        alternate-ranks (map (fn [r] (if (= 14 r) 1 r)) ranks)]
    (or (straight-by-ranks? ranks) (straight-by-ranks? alternate-ranks))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        hand-values (map second (filter (fn [checker] ((first checker) hand)) checkers))]
    (apply max hand-values)))
