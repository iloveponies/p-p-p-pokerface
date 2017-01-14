(ns p-p-p-pokerface)

(def letter-to-rank
  "A mapping between letters, representing cards, and their numerical ranks."
  {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank
  "Get off a rank from this card."
  [card]
  (let [[rank-symbol _] card]
     (if
       (Character/isDigit rank-symbol)
       (Integer/parseInt (str rank-symbol))
       (get letter-to-rank rank-symbol))))

(defn suit
  "Get off a suit from this card."
  [card]
  (let [[_ suit-symbol] card] (str suit-symbol)))

(defn kind-of?
  "Determine, if this hand contains given number of cards of a same rank."
  [hand, number]
  (not
    (empty?
      (filter
        (fn [rank-frequency] (<= number rank-frequency))
        (vals (frequencies (map rank hand)))))))

(defn pair?
  "Determine, if this hand contains a pair."
  [hand]
  (kind-of? hand 2))

(defn three-of-a-kind?
  "Determine, if this hand contains three of a kind."
  [hand]
  (kind-of? hand 3))

(defn four-of-a-kind?
  "Determine, if this hand contains four of a kind."
  [hand]
  (kind-of? hand 4))

(defn flush?
  "Determine, if this hand contains a flush."
  [hand]
  (== 1 (count (set (map suit hand)))))

(defn rank-frequencies-equals
  "Compare rank frequencies for given hand, with sample sequence of rank freqs.
  This function implicate, that sample sequence is sorted"
  [hand sample]
  (= sample (sort (vals (frequencies (map rank hand))))))

(defn full-house?
  "Determine, if this hand contains full house or not."
  [hand]
  (rank-frequencies-equals hand [2 3]) )

(defn two-pairs?
  "Determine, if this hand contains two pairs or not."
  [hand]
  (rank-frequencies-equals hand [1 2 2]) )

(def low-ace-straight-ranking
  "An array sequence, defining ranking of bicycle straight."
  (seq [2 3 4 5 14]))

(defn straight?
  "Determine, if this hand straight or not."
  [hand]
  (let
    [present-ranks-sorted (sort (map rank hand))
     smallest-rank (first present-ranks-sorted)]
    (or
      (= (range smallest-rank (+ 5 smallest-rank)) present-ranks-sorted)
      (= present-ranks-sorted low-ace-straight-ranking))))

(defn straight-flush?
  "Determine, if this hand contains straight flush, or not."
  [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card?
  "Every hand contains high-card."
  [hand]
  true)

(defn value
  "Get a value of this hand."
  [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        checker-carrier (fn
                  [checker-pair]
                  (let
                    [[checker-func return-value] checker-pair]
                    (if (checker-func hand) return-value -1)))]
     (apply max (set (map checker-carrier checkers)))))
