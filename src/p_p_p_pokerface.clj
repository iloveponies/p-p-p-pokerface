(ns p-p-p-pokerface)

; A card has a rank and a suit.
; Rank -> 2..10, J, Q, K, A
; Suit -> Clubs, Diamond, Hearts and Spades

(defn rank [card]
  (let [[fst _] card
        replacements {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (get replacements fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn number-of-occurences 
  "Return the number of occurence of n cards"
  [hand n]
  (let [v (vals (frequencies (map rank hand)))
        p (filter (fn [x] (== x n)) v)]
    (count (vec p))))

(defn pair? [hand]
  (== 1 (number-of-occurences hand 2)))

(defn three-of-a-kind? [hand]
  (== 1 (number-of-occurences hand 3)))

(defn four-of-a-kind? [hand]
  (== 1 (number-of-occurences hand 4)))

(defn flush?
  "All five cards are of the same suit."
  [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (and (pair? hand)
       (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (== 2 (number-of-occurences hand 2)))

(defn straight? [hand]
  (let [helper-straight? (fn [h]
                           (let [sorted-hand (sort (vec (map rank h)))
                                 f (first sorted-hand)]
                             (= sorted-hand (range f (+ f 5)))))]
    (or (helper-straight? hand)
        (helper-straight? (replace {"AC" "1C",
                                    "AD" "1D",
                                    "AH" "1H",
                                    "AS" "1S"} hand)))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

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
    :else 0
    ))
