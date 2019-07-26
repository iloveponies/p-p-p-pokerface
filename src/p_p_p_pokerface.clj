;;; --------------------------------------------------------------------
(ns p-p-p-pokerface)

(def rank->int {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [[rank _]]
  (if (Character/isDigit rank)
    (Integer/valueOf (str rank))
    (rank->int rank)))

(defn suit [[_ suit]]
  (str suit))

(defn component-frequencies [component, hand]
  (vals (frequencies (map component hand))))

(defn n-of-a-kind? [n, hand]
  (let [rank-frequencies (component-frequencies rank hand)]
    (>= (apply max rank-frequencies) n)))

(defn pair? [hand]
  (n-of-a-kind? 2 hand))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? 3 hand))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? 4 hand))

(defn flush? [hand]
  (let [suit-frequencies (vals (frequencies (map suit hand)))]
    (= (first suit-frequencies) 5)))

(defn full-house? [hand]
  (let [rank-frequencies (component-frequencies rank hand)]
      (every? #{2 3} rank-frequencies)))

(defn two-pairs? [hand]
  (let [rank-frequencies (component-frequencies rank hand)
        pair-count ((frequencies rank-frequencies) 2)]
    (= pair-count 2)))

(defn sorted-straight? [ranks]
  (let [fst (first ranks)]
    (= (range fst (+ fst 5)) ranks)))

(defn straight? [hand]
  (let [high-ace-ranks (sort (map rank hand))
        low-ace-ranks (sort (replace {14 1} high-ace-ranks))]
    (or (sorted-straight? high-ace-ranks)
        (sorted-straight? low-ace-ranks))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(def hand-checkers [(fn [hand] true)
                    pair?
                    two-pairs?
                    three-of-a-kind?
                    straight?
                    flush?
                    full-house?
                    four-of-a-kind?
                    straight-flush?])

(defn value [hand]
  (let [val-bool-pairs (map-indexed #(vector %1 (%2 hand))
                                    hand-checkers)
        true-pairs (filter second val-bool-pairs)]
    (apply max (map first true-pairs))))
