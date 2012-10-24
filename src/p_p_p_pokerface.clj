(ns p-p-p-pokerface)

(def highranks {\T 10
                \J 11
                \Q 12
                \K 13
                \A 14})

(defn rank
  "Returns the rank of a card as an integer."
  [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst)) ;; Convert char to integer and return
      (highranks fst))))          ;; Look up the integer value of a high rank

(defn suit
  "Returns the suit of a card as a string.
   (H)earts
   (S)pades
   (C)lubs
   (D)iamonds"
  [card]
  (let [[_ snd] card]
    (str snd)))

(defn number-of-n-of-a-kind
  "Returns the number of n of a kinds in a hand (e.g. number of pairs if n=2)."
  [n hand]
  (count (filter (fn [x] (= x n)) (vals (frequencies (map rank hand))))))

(defn pair? 
  "Checks if a hand has a pair (or more)."
  [hand]
  (> (number-of-n-of-a-kind 2 hand)
     0))

(defn three-of-a-kind? 
  "Checks if a hand has three of a kind."
  [hand]
  (> (number-of-n-of-a-kind 3 hand)
     0))

(defn four-of-a-kind? 
  "Checks if a hand has four of a kind."
  [hand]
  (> (number-of-n-of-a-kind 4 hand)
     0))

(defn flush? 
  "Checks if all the cards in a hand are of the same suit."
  [hand]
  (= (count (vals (frequencies (map suit hand))))
  1))

(defn full-house? 
  "Checks if a hand has both a pair and three of a kind."
  [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? 
  "Checks if a hand has two pairs (two cards of same rank and two of other matching rank).
   Fails on a four-of-a-kind hand, as it should."
  [hand]
  (= (number-of-n-of-a-kind 2 hand)
     2))

(defn straight? 
  "Checks if a hand has a straight (5 consecutive ranks)."
  [hand]
  (let [lowest-rank (apply min (map rank hand))     ;; Find the lowest ranking card
        ordered-cards (sort (map (fn [x] (- x lowest-rank))
                                 (map rank hand)))] ;; Substract the lowest rank from all (sorted) cards
    (or (= ordered-cards
           '(0 1 2 3 4))                            ;; ... and compare to 0...4 sequence.
        (= ordered-cards 
           '(0 1 2 3 12)))))                        ;; ... and compare to 0...3, 12 sequence (low straight, Ace = 1).

(defn straight-flush? 
  "Checks if a hand has both a straight and a flush."
  [hand]
  (and (straight? hand) (flush? hand)))

(defn value 
  "Checks if a hand has anything of value (in descending order). If nothing found, returns 0 (only high card)."
  [hand]
  (cond
    (straight-flush? hand)  8
    (four-of-a-kind? hand)  7
    (full-house? hand)      6
    (flush? hand)           5
    (straight? hand)        4
    (three-of-a-kind? hand) 3
    (two-pairs? hand)       2
    (pair? hand)            1
    :else                   0))
