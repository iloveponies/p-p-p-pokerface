(ns p-p-p-pokerface)

(def rank-value {\T 10 \J 11 \Q 12 \K 13 \A 14})

(defn rank [card]
  (let [[x _] card]
    (if (Character/isDigit x)
      (Integer/valueOf (str x))
      (rank-value x))))

(defn suit [card]
  (let [[_ x] card]
    (str x)))

(defn rank-freqs [hand]
  (let [ranks (map rank hand)]
    (vals (frequencies ranks))))

(defn max-of-a-kind? [hand n]
  (= n (apply max (rank-freqs hand))))

(defn high-card? [hand]
  true)

(defn pair? [hand]
  (max-of-a-kind? hand 2))

(defn three-of-a-kind? [hand]
  (max-of-a-kind? hand 3))

(defn four-of-a-kind? [hand]
  (max-of-a-kind? hand 4))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (apply = suits)))

(defn full-house? [hand]
  (let [rank-freqs (sort (rank-freqs hand))]
    (= [2 3] rank-freqs)))

(defn two-pairs? [hand]
  (let [rank-freqs (sort (rank-freqs hand))]
    (or  (= [1 2 2] rank-freqs)
         (= [1 4] rank-freqs))))

(defn next-numbers? [a-seq]
  (let [a-min (apply min a-seq)
        proper-seq (range a-min (+ a-min 5))]
    (= a-seq proper-seq)))

(defn straight? [hand]
  (let [ace-high-ranks (sort (map rank hand))
        ace-low-ranks (sort (replace {14 1} ace-high-ranks))]
    (or (next-numbers? ace-high-ranks)
        (next-numbers? ace-low-ranks))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        hand-score (fn [[pred? score]] (if (pred? hand) score 0))
        scores (map hand-score checkers)]
    (apply max scores)))
