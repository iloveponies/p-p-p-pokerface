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

(defn straight-without-replacement? [sorted-ranks start-index]
  (let [straight-to-check (seq (range start-index (+ start-index 5)))]
    (cond
      (> start-index 10) false
      (= straight-to-check sorted-ranks) true
      :else (straight-without-replacement? sorted-ranks (+ start-index 1)))))

(defn straight? [hand]
  (let [sorted (sorted-ranks hand)
        ace-1 (replace {14 1} sorted)
        ace-1-sorted (sort ace-1)]
    (cond
      (= ace-1-sorted (seq (range 1 6))) true
      :else (straight-without-replacement? sorted 1))))

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)

