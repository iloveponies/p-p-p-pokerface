(ns p-p-p-pokerface)

(defn rank [card]
  (let [[raw-rank _] card
        replacements { \T 10 \J 11 \Q 12 \K 13 \A 14 }
        replacement-rank (get replacements raw-rank raw-rank)]
    (if (Character/isDigit replacement-rank)
      (Integer/valueOf (str replacement-rank))
      replacement-rank)))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn n-of-a-kind? [n hand]
  (let [ranks (map rank hand)
        freqs (set (vals (frequencies ranks)))]
    (contains? freqs n)))

(defn pair? [hand]
  (n-of-a-kind? 2 hand))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? 3 hand))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? 4 hand))

(defn flush? [hand]
  (let [suits (map suit hand)
        freqs (vals (frequencies suits))]
    (= 5 (apply max freqs))))

(defn full-house? [hand]
  (and (pair? hand)
       (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))
        sorted-freqs (seq (sort freqs))]
    (= [1 2 2] sorted-freqs)))

(defn straight? [hand]
  (let [suits (map suit hand)
        suit-freq (vals (frequencies suits))
        ranks (map rank hand)
        min-rank (apply min ranks)
        max-rank (+ 1 (apply max ranks))
        sorted-ranks (sort ranks)
        expected-ranks (range min-rank max-rank)
        replaced-ranks (replace { 14 1 } ranks)
        low-sorted-ranks (sort replaced-ranks)
        low-min-rank (apply min low-sorted-ranks)
        low-max-rank (+ 1 (apply max low-sorted-ranks))
        low-expected-ranks (range low-min-rank low-max-rank)]
    (or (= expected-ranks sorted-ranks)
        (= low-expected-ranks low-sorted-ranks))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

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
    :else 0))
