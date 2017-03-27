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
        ace-1 (replace {14 1} sorted)
        ace-1-sorted (sort ace-1)]
    (cond
      (= ace-1-sorted (seq (range 1 6))) true
      (= sorted (seq (range 2 7))) true
      (= sorted (seq (range 3 8))) true
      (= sorted (seq (range 4 9))) true
      (= sorted (seq (range 5 10))) true
      (= sorted (seq (range 6 11))) true
      (= sorted (seq (range 7 12))) true
      (= sorted (seq (range 8 13))) true
      (= sorted (seq (range 9 14))) true
      (= sorted (seq (range 10 15))) true
      :else false)))

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)

