(ns p-p-p-pokerface)

(def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[rankchar _] card
        is-digit (Character/isDigit rankchar)]
    (if is-digit (Integer/valueOf (str rankchar)) (replacements rankchar))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn sorted-ranks [hand]
  (sort (map rank hand)))

(defn frequency-values [hand]
    (vals (frequencies (sorted-ranks hand))))

(defn pair? [hand]
  (contains? (set (frequency-values hand)) 2))

(defn three-of-a-kind? [hand]
  (contains? (set (frequency-values hand)) 3))

(defn four-of-a-kind? [hand]
  (contains? (set (frequency-values hand)) 4))

(defn flush? [hand]
  (let [suits (map suit hand)
        suit-count (count (set suits))]
    (== suit-count 1)))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn pure-two-pairs? [hand]
  (let [sorted-freq-vals (sort (frequency-values hand))]
    (= [1 2 2] sorted-freq-vals)))

(defn two-pairs? [hand]
  (let [pure-two-pair false]
    (cond
      (four-of-a-kind? hand) true
      (pure-two-pairs? hand) true
      :else false)))

(defn straight? [hand]
  (let [sorted (sorted-ranks hand)
        min-rank (apply min sorted)
        ace-1 (replace {14 1} sorted)
        ace-1-sorted (sort ace-1)]
    (cond
      (= ace-1-sorted (seq (range 1 6))) true
      :else (= sorted (seq (range min-rank (+ min-rank 5)))))))

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
    :else 0))

