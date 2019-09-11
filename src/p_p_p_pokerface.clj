(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank-char _] card
        rank-map {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit rank-char)
      (Integer/valueOf (str rank-char))
      (get rank-map rank-char))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn- max-same-rank [hand]
  (apply max (vals (frequencies (map rank hand)))))

(defn pair? [hand]
  (let [max-same-rank (max-same-rank hand)]
    (if (= 2 max-same-rank)
      true
      false)))

(defn three-of-a-kind? [hand]
  (let [max-same-rank (max-same-rank hand)]
    (if (= 3 max-same-rank)
      true
      false)))

(defn four-of-a-kind? [hand]
  (let [max-same-rank (max-same-rank hand)]
    (if (= 4 max-same-rank)
      true
      false)))

(defn max-same-suit [hand]
  (apply max (vals (frequencies (map suit hand)))))

(defn flush? [hand]
  (let [max-same-suit (max-same-suit hand)]
    (if (= (count hand) max-same-suit)
      true
      false)))

(defn full-house? [hand]
  (let [rank-frequencies (vals (frequencies (map rank hand)))]
    (if (= (seq [2 3]) (sort rank-frequencies))
      true
      false)))

(defn two-pairs? [hand]
  (let [rank-frequencies (vals (frequencies (map rank hand)))]
    (if (or
         (= (seq [1 2 2]) (sort rank-frequencies))
         (four-of-a-kind? hand))
      true
      false)))

(defn straight? [hand]
  (let [sorted-ranks (sort (map rank hand))
        sorted-ranks-ace-low (sort (replace {14 1} sorted-ranks))
        first-rank (first sorted-ranks)
        straight (range first-rank (+ first-rank 5))
        straight-ace-low (range (first sorted-ranks-ace-low) (+ (first sorted-ranks-ace-low) 5))]
    (if (or (= sorted-ranks straight)
            (= sorted-ranks-ace-low straight-ace-low))
    true
    false)))

(defn straight-flush? [hand]
  (if (and (straight? hand)
           (flush? hand))
    true
    false))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1]
                   [two-pairs? 2] [three-of-a-kind? 3]
                   [straight? 4] [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        checker-applies (fn [[f _]] (f hand))]
    (apply max (map second (filter checker-applies checkers)))))
