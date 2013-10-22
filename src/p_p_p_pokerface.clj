(ns p-p-p-pokerface)

; Hands for testing
;; (def high-seven                   ["2H" "3S" "4C" "5C" "7D"])
;; (def pair-hand                    ["2H" "2S" "4C" "5C" "7D"])
;; (def two-pairs-hand               ["2H" "2S" "4C" "4D" "7D"])
;; (def three-of-a-kind-hand         ["2H" "2S" "2C" "4D" "7D"])
;; (def four-of-a-kind-hand          ["2H" "2S" "2C" "2D" "7D"])
;; (def straight-hand                ["2H" "3S" "6C" "5D" "4D"])
;; (def low-ace-straight-hand        ["2H" "3S" "4C" "5D" "AD"])
;; (def high-ace-straight-hand       ["TH" "AS" "QC" "KD" "JD"])
;; (def flush-hand                   ["2H" "4H" "5H" "9H" "7H"])
;; (def full-house-hand              ["2H" "5D" "2D" "2C" "5S"])
;; (def straight-flush-hand          ["2H" "3H" "6H" "5H" "4H"])
;; (def low-ace-straight-flush-hand  ["2D" "3D" "4D" "5D" "AD"])
;; (def high-ace-straight-flush-hand ["TS" "AS" "QS" "KS" "JS"])

(def high-ranks {\T 10
                 \J 11
                 \Q 12
                 \K 13
                 \A 14})
(defn rank
  "Returns the rank of a card as an integer."
  [card]
  (let [[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank)) ;; Convert char to integer and return
      (high-ranks rank))))         ;; Else look up the integer value of a high rank

(defn suit [card]
  "Returns the suit as a one-letter string."
  (let [[_ suit] card]
    (str suit)))

(defn number-of-n-of-a-kind
  "Returns the number of n of a kinds in a hand (e.g. number of pairs if n=2)."
  [n hand]
  (count (filter (fn [x] (= x n)) (vals (frequencies (map rank hand))))))

(defn pair?
  "Checks if a hand has a pair (or more)."
  [hand]
  (> (number-of-n-of-a-kind 2 hand) 0))

(defn three-of-a-kind?
  "Checks if a hand has three of a kind."
  [hand]
  (> (number-of-n-of-a-kind 3 hand) 0))

(defn four-of-a-kind?
  "Checks if a hand has four of a kind."
  [hand]
  (> (number-of-n-of-a-kind 4 hand) 0))

(defn flush? [hand]
  "Checks if only one suit is found."
  (== 1 (count (set (map suit hand)))))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  "Checks if a hand has two pairs (or more)."
  [hand]
  (> (number-of-n-of-a-kind 2 hand) 1))

(defn straight?
  "Checks if a hand has a straight (5 consecutive ranks)."
  [hand]
  (let [lowest-rank (apply min (map rank hand))     ;; Find the lowest ranking card
        ordered-cards (sort (map (fn [x] (- x lowest-rank))
                                 (map rank hand)))] ;; Substract the lowest rank from all (sorted) cards
    (or (= ordered-cards
           '(0 1 2 3 4))                            ;; ... then compare to 0...4 sequence.
        (= ordered-cards
           '(0 1 2 3 12)))))                        ;; ... or compare to 0...3, 12 sequence (low straight, Ace = 1).

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value
  "Checks if a hand has anything of value (in descending order). If nothing found, returns 0 (only high card)."
  [hand]
  (cond
    (straight-flush?  hand) 8
    (four-of-a-kind?  hand) 7
    (full-house?      hand) 6
    (flush?           hand) 5
    (straight?        hand) 4
    (three-of-a-kind? hand) 3
    (two-pairs?       hand) 2
    (pair?            hand) 1
    :else                   0))
