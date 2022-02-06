(ns p-p-p-pokerface)

(defn rank [card]
  (let [rank (first card)
        rank-map {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (rank-map rank))
    )
  )

(defn ranks-freq [hand]
  (frequencies (map rank hand)))

(defn repeats-ranks-freq? [hand freq repeats]
  (let [rf-seq (vals (ranks-freq hand))]
    (== (count (filter (fn [n] (== n freq)) rf-seq)) repeats)
    )
  )

(defn suit [card]
  (let [[_ suit] card]
    (str suit))
  )

(defn high-card? [_]
  true)

(defn pair? [hand]
  (repeats-ranks-freq? hand 2 1))

(defn three-of-a-kind? [hand]
  (repeats-ranks-freq? hand 3 1))

(defn four-of-a-kind? [hand]
  (repeats-ranks-freq? hand 4 1))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (= (count (set suits)) 1)
    ))

(defn full-house? [hand]
  (and (repeats-ranks-freq? hand 3 1) (repeats-ranks-freq? hand 2 1))
  )

(defn two-pairs? [hand]
  (or (repeats-ranks-freq? hand 2 2) (four-of-a-kind? hand)))

(defn straight? [hand]
  (let [s_ranks (sort (map rank hand))
        ascending? (fn [x] (= x (range (first x) (inc (last x)))))]
    (or
      (ascending? s_ranks)
      (ascending? (sort (replace {14 1} s_ranks))))))

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
    (high-card? hand) 0
    :else             -1)
  )
