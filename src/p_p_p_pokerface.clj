(ns p-p-p-pokerface)

(defn
  rank
  "Returns the rank of the card."
  [card]
  (let [[^char rank _] card
        digit? (Character/isDigit rank)
        values {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if digit?
      (Integer/valueOf (str rank))
      (values rank))))

(defn
  suit
  "Returns the suit of the card."
  [card]
  (let [[_ suit] card]
    (str suit)))

(defn
  unique-suits
  "Returns the amount of different suits in the hand."
  [hand]
  (count (frequencies (map suit hand))))

(defn
  unique-ranks
  "Returns the amount of different ranks in the hand."
  [hand]
  (count (frequencies (map rank hand))))

(defn
  amount-of-pairs
  "Returns the amount of pairs in the hand"
  [hand]
  (let [ranks (frequencies (map rank hand))]
    (count (filter #{2} (vals ranks)))))

(defn pair?
  "Returns true if the hand includes a pair"
  [hand]
  (< 0 (amount-of-pairs hand)))

(defn
  three-of-a-kind?
  "Returns true if the hand has three of a kind."
  [hand]
  (and (== 3 (unique-ranks hand)) (not (pair? hand))))

(defn
  four-of-a-kind?
  "Returns true if the hand has four of a kind."
  [hand]
  (and (== 2 (unique-ranks hand)) (not (pair? hand))))

(defn
  flush?
  "Returns true if the hand has a flush."
  [hand]
  (== 1 (unique-suits hand)))

(defn
  full-house?
  "Returns true if the hand has a full house."
  [hand]
  (and (== 2 (unique-ranks hand)) (== 1 (amount-of-pairs hand))))

(defn
  two-pairs?
  "Returns true if the hand has two pairs."
  [hand]
  (or (== 2 (amount-of-pairs hand)) (four-of-a-kind? hand)))

(defn
  straight?
  "Returns true if the hand has a straight."
  [hand]
  (let [ranks (map rank hand)
        min-rank (apply min ranks)
        small-ace? (== 2 min-rank)
        updated-ranks (if small-ace?
                            (replace {14 1} ranks)
                            ranks)
        sorted-hand (sort updated-ranks)
        updated-min-rank (apply min sorted-hand)
        comparison-hand (range updated-min-rank (+ updated-min-rank 5))]
    (= comparison-hand sorted-hand)))

(defn
  straight-flush?
  "Returns true if the hand has a straight flush."
  [hand]
  (and (flush? hand) (straight? hand)))

(defn value [hand]
  nil)
