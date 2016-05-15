(ns p-p-p-pokerface)

(def replacement {\T 10 \J 11 \Q 12 \K 13 \A 14})

(defn rank [[rank suit]]
  (if (Character/isDigit rank) (Integer/valueOf (str rank)) (replacement rank)))

(defn suit [[rank suit]]
  (str suit))

(defn pair? [hand]
  (let [ranks (map first hand)
        rank-distribution (frequencies ranks)
        ocurrences (set (vals rank-distribution))]
    (contains? ocurrences 2)))

(defn three-of-a-kind? [hand]
  (let [ranks (map first hand)
        rank-distribution (frequencies ranks)
        ocurrences (set (vals rank-distribution))]
    (contains? ocurrences 3)))

(defn four-of-a-kind? [hand]
  (let [ranks (map first hand)
        rank-distribution (frequencies ranks)
        ocurrences (set (vals rank-distribution))]
    (contains? ocurrences 4)))

(defn flush? [hand]
  (let [suits (map second hand)
        suit-distribution (frequencies suits)]
    (= (count suit-distribution) 1)))

(defn full-house? [hand]
  (let [ranks (map first hand)
        rank-distribution (frequencies ranks)
        ocurrences (set (vals rank-distribution))]
    (= (sort ocurrences) '(2 3))))

(defn two-pairs? [hand]
  (let [ranks (map first hand)
        rank-distribution (frequencies ranks)
        counts (vals rank-distribution)
        distribution-counts (frequencies counts)
        has-two-pairs (= 2 (get distribution-counts 2))
        has-four-same (get distribution-counts 4)]
    (if (or has-two-pairs has-four-same) true false)))


(defn consecutive? [ranks]
  (let [min-rank (apply min ranks)
        max-rank (apply max ranks)
        total-cards-minus-one (- (count ranks) 1)]
    (if (= max-rank (+ min-rank total-cards-minus-one)) true false)))

(defn is-ace? [rank]
  (if (= rank 14) true false))


(defn straight? [hand]
  (let [all-ranks (set (map rank hand))
        num-uniques (count all-ranks)
        is-enough-cards (= num-uniques 5)
        has-ace (contains? (set all-ranks) 14)
        with-1 (if has-ace (conj (remove is-ace? all-ranks) 1) all-ranks)
        is-consecutive (or (consecutive? all-ranks) (consecutive? with-1))]
    (if (and is-consecutive is-enough-cards) true false)))

(defn straight-flush? [hand]
  (if (and (straight? hand) (flush? hand)) true false))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        get-value (fn [checker] (if (apply (first checker) [hand])
                                       (second checker) 0))]
    (reduce max (map get-value checkers))))