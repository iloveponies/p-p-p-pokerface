(ns p-p-p-pokerface)

(defn rank [card]
  (let [ranks {\T 10, \J 11, \Q 12, \K 13, \A 14}
        [card-rank _] card]
    (if (Character/isDigit card-rank)
      (Integer/valueOf (str card-rank))
      (ranks card-rank))))

(defn suit [card]
  (let [[_ card-suit] card]
    (str card-suit)))


; Helper functions

(defn rank-frequencies
  "Returns a map from ranks in a hand to the number of times they appear."
  [hand]
  (let [hand-ranks (map rank hand)]
    (frequencies hand-ranks)))

(defn max-of-a-kind
  "Returns the greatest quantity of cards with mutual ranks in a hand."
  [hand]
  (let [rank-freqs (rank-frequencies hand)]
    (apply max (vals rank-freqs))))

(defn sorted-counts
  "Returns the sorted counts of cards with mutual ranks."
  [hand]
  (let [rank-freqs (rank-frequencies hand)]
    (sort (vals rank-freqs))))


(defn pair? [hand]
  (>= (max-of-a-kind hand) 2))

(defn three-of-a-kind? [hand]
  (>= (max-of-a-kind hand) 3))

(defn four-of-a-kind? [hand]
  (== (max-of-a-kind hand) 4))

(defn flush? [hand]
  (let [hand-suits (map suit hand)]
    (== (count (set hand-suits)) 1)))

(defn full-house? [hand]
  (let [sorted-cs (sorted-counts hand)]
    (= sorted-cs [2 3])))

(defn two-pairs? [hand]
  (let [sorted-cs (sorted-counts hand)]
    (or
      (= sorted-cs [1 2 2])
      (= sorted-cs [1 4]))))

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
