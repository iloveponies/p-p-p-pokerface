(ns p-p-p-pokerface)

(def rank-characters {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[rank-of-card _] card]
    (if (Character/isDigit rank-of-card)
      (Integer/valueOf (str rank-of-card))
      (rank-characters rank-of-card))))

(defn suit [card]
  (let [[_ suit-of-card] card]
    (str suit-of-card)))

(defn max-rank-frequency [hand]
  (let [ranks-of-hand (map rank hand)]
    (apply max (vals (frequencies ranks-of-hand)))))

(defn pair? [hand]
  (>= (max-rank-frequency hand) 2))

(defn three-of-a-kind? [hand]
  (>= (max-rank-frequency hand) 3))

(defn four-of-a-kind? [hand]
  (>= (max-rank-frequency hand) 4))

(defn flush? [hand]
  (let [suits-of-hand (map suit hand)
        max-suit-freq (apply max (vals (frequencies suits-of-hand)))]
    (= max-suit-freq 5)))

(defn full-house? [hand]
  (let [sorted-ranks-of-hand (sort (map rank hand))
        [c1 c2 c3 c4 c5]     sorted-ranks-of-hand]
    (or
      (and (== c1 c2 c3) (== c4 c5))
      (and (== c1 c2) (== c3 c4 c5)))))

(defn two-pairs? [hand]
  (let [reversed-rank-frequencies (reverse (sort (vals (frequencies (map rank hand)))))
        comparison-frequencies    (seq [2 2 1])]
    (= reversed-rank-frequencies comparison-frequencies)))

(defn straight? [hand]
  (let [ranks                (map rank hand)
        sorted-ranks1        (sort ranks)
        sorted-ranks2        (sort (replace {14 1} ranks))
        min-rank1            (first sorted-ranks1)
        min-rank2            (first sorted-ranks2)
        comparison-ranks1    (range min-rank1 (+ min-rank1 5))
        comparison-ranks2    (range min-rank2 (+ min-rank2 5))]
    (or
      (= sorted-ranks1 comparison-ranks1)
      (= sorted-ranks2 comparison-ranks2))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn value [hand]
    (let [checkers #{[high-card? 0]  [pair? 1]
                     [two-pairs? 2]  [three-of-a-kind? 3]
                     [straight? 4]   [flush? 5]
                     [full-house? 6] [four-of-a-kind? 7]
                     [straight-flush? 8]}]
      (apply max (map second (filter (fn [checker] ((first checker) hand)) checkers)))))
