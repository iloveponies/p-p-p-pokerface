(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card
        rank-map {\2 2,
                  \3 3,
                  \4 4,
                  \5 5,
                  \6 6,
                  \7 7,
                  \8 8,
                  \9 9,
                  \T 10,
                  \J 11,
                  \Q 12,
                  \K 13,
                  \A 14}]
    (get rank-map r)))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn of-a-kind [hand num part-fn res-fn]
  (let [map-fn (fn [x] (part-fn x))
        pair-filter (fn [x] (= x num))]
    (res-fn
      (filter pair-filter (vals (frequencies (map map-fn hand)))))))

(defn pair? [hand]
  (of-a-kind hand 2 rank (fn [x] (= (count x) 1))))

(defn three-of-a-kind? [hand]
  (of-a-kind hand 3 rank (fn [x] (= (count x) 1))))

(defn four-of-a-kind? [hand]
  (of-a-kind hand 4 rank (fn [x] (= (count x) 1))))

(defn flush? [hand]
  (of-a-kind hand 5 suit (fn [x] (= (count x) 1))))

(defn full-house? [hand]
  (let [ranks (map rank hand)]
    (= (count (set ranks)) 2)))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        freq (frequencies ranks)
        just-freq (map (fn [x] (val x)) freq)
        tail-freqs (rest (sort just-freq))]
    (or (= (count tail-freqs) 2)
        (and (= (count tail-freqs) 1)
             (= (first tail-freqs) 4)))))

(defn straight-high-ace? [hand]
  (let [sorted-ranks (sort hand)
        rank-range (range (first sorted-ranks) (+ (first sorted-ranks) (count sorted-ranks)))]
    (= sorted-ranks rank-range)))

(defn straight-low-ace? [hand]
  (let [sorted-ranks (sort (map (fn [x] (if (= x 14) 1 x)) hand))
        rank-range (range (first sorted-ranks) (+ (first sorted-ranks) (count sorted-ranks)))]
    (= sorted-ranks rank-range)))

(defn straight? [hand]
  (let [suit-freq-count (count (frequencies (map suit hand)))
        ranks (map rank hand)]
    (and (> suit-freq-count 1)
         (or (straight-high-ace? ranks) (straight-low-ace? ranks)))))

(defn straight-flush? [hand]
  (let [suit-freq-count (count (frequencies (map suit hand)))
        ranks (map rank hand)]
    (and (= suit-freq-count 1)
         (or (straight-high-ace? ranks) (straight-low-ace? ranks)))))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1]
                   [two-pairs? 2] [three-of-a-kind? 3]
                   [straight? 4] [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        value-for (fn [x] (if ((first x) hand) (second x) 0))]
    (last (sort (map value-for checkers)))))
