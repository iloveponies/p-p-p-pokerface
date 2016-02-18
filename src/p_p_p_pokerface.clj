(ns p-p-p-pokerface)

(defn rank [[card-rank _]]
  (let [replacements  {\T 10
                       \J 11
                       \Q 12
                       \K 13
                       \A 14}]
    (if (contains? replacements card-rank)
      (replacements card-rank)
      (Integer/valueOf (str card-rank)))))

(defn suit [[_ card-suit]]
  (str card-suit))

(defn ranks [hand]
  "returns a sorted list of the ranks in a hand"
  (sort (map (fn [card] (rank card)) hand)))

(defn rank-groups [hand]
  "returns a sorted list of the rank groupings in a hand"
  (sort (vals (frequencies (ranks hand)))))

(defn suits [hand]
  "returns a list of the suits in a hand"
  (map (fn [card] (suit card)) hand))

(defn pair? [hand]
  (=
   (apply max (rank-groups hand))
   2))

(defn three-of-a-kind? [hand]
  (=
   (apply max (rank-groups hand))
   3))

(defn four-of-a-kind? [hand]
  (=
   (apply max (rank-groups hand))
   4))

(defn flush? [hand]
  (=
   (apply max (vals (frequencies (suits hand))))
   5))

(defn full-house? [hand]
  (=
   (rank-groups hand)
   [2 3]))

(defn two-pairs? [hand]
  (=
   (rank-groups hand)
   [1 2 2]))

(defn high-rank [hand]
  "returns the rank of the highest card in the hand"
  (apply max (ranks hand)))

(defn low-rank [hand]
  "returns the rank of the lowest card in the hand"
  (apply min (ranks hand)))

(defn rank-range [hand]
  "returns the range of ranks in the hand"
  (range (low-rank hand) (inc (high-rank hand))))

(defn low-ace-ranks [hand]
  "returns an ace hand's ranks sorted with the ace low"
  (sort (replace {14 1} (ranks hand))))

(defn straight? [hand]
  (or
   (= (ranks hand) (rank-range hand))
   (= (low-ace-ranks hand)
      (range (apply min (low-ace-ranks hand))
             (inc (apply max (low-ace-ranks hand)))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1]
                   [two-pairs? 2] [three-of-a-kind? 3]
                   [straight? 4] [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max ( map (fn [[_ value]] value)
                     (filter (fn [[test _]] (test hand)) checkers)))))
