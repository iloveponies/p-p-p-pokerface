(ns p-p-p-pokerface)

(defn rank [card]
  (let [[a] card
        card-value {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit a)
      (Integer/valueOf (str a))
      (card-value a))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn rank-freqs [hand]
  (vals (frequencies (map rank hand))))

(defn freqs-in-order [hand]
  (reverse (sort (rank-freqs hand))))

(defn same-cards-exist? [hand amount]
  (<= amount (apply max (rank-freqs hand))))

(defn pair? [hand]
  (same-cards-exist? hand 2))

(defn three-of-a-kind? [hand]
  (same-cards-exist? hand 3))

(defn four-of-a-kind? [hand]
  (same-cards-exist? hand 4))

(defn flush? [hand]
  (= 1(count (frequencies (map suit hand)))))

(defn full-house? [hand]
  (let [freqs (freqs-in-order hand)]
    (and (= 3 (first freqs)) (= 2 (first (rest freqs))))))

(defn two-pairs? [hand]
  (let [freqs (freqs-in-order hand)]
    (or (= 4 (first freqs))
        (and (= 2 (first freqs))
             (= 2 (first (rest freqs)))))))

(defn straight? [hand]
  (let [sorted (sort (map rank hand))
        change (fn [v] (replace {14 1} v))
        min (fn [v] (apply min v))]
    (or (= sorted
           (range (min sorted) (+ (min sorted) 5)))
        (= (sort(change sorted))
           (range (min (change sorted)) (+ (min (change sorted)) 5))))
    ))

(defn straight-flush? [hand]
  (and (straight? hand)(flush? hand)))

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
