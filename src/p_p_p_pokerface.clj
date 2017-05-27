(ns p-p-p-pokerface)

(defn rank [[the_rank _]]
  (let [replacements {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit the_rank)
      (Integer/valueOf (str the_rank))
      (get replacements the_rank))))

(defn suit [[_ the_suit]]
  (str the_suit))

(defn sorted-rank-frequencies [hand]
  (sort (vals (frequencies (map rank hand)))))

(defn pair? [hand]
  (= [1 1 1 2] (sorted-rank-frequencies hand)))

(defn three-of-a-kind? [hand]
  (= [1 1 3] (sorted-rank-frequencies hand)))

(defn four-of-a-kind? [hand]
  (= [1 4] (sorted-rank-frequencies hand)))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (= [2 3] (sorted-rank-frequencies hand)))

(defn two-pairs? [hand]
  (= [1 2 2] (sorted-rank-frequencies hand)))

(defn sequential-ranks? [hand]
  (let [sorted-ranks (sort (map rank hand))
        last-rank (last sorted-ranks)
        first-rank (first sorted-ranks)
        low-ace? (= '(2 3 4 5 14) sorted-ranks)
        other-sequential-ranks? (and (apply < sorted-ranks) (= 4 (- last-rank first-rank)))
        ]
    (or low-ace? other-sequential-ranks?)))

(defn same-suit? [hand]
  (apply = (map suit hand)))

(defn straight? [hand]
  (let [suits (map suit hand)]
    (and (not (same-suit? hand)) (sequential-ranks? hand))))

(defn straight-flush? [hand]
  (let [suits (map suit hand)]
    (and (same-suit? hand) (sequential-ranks? hand))))

(defn high-hand? [_] true)

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
    (high-hand? hand) 0))

