(ns p-p-p-pokerface)

;; To get the rank, you’ll need to convert a character into an integer. To see if a character is a digit, like \5 or \2, you can use (Character/isDigit char):
;; (Character/isDigit \5) ;=> true
;; (Character/isDigit \A) ;=> false
;; If a character is a digit, you can use (Integer/valueOf string) to convert it to an integer. You will first have to convert the character into a string.
;; (Integer/valueOf "12")     ;=> 12
;; (Integer/valueOf (str \5)) ;=> 5
;; Finally, to turn the characters T, J, Q, K and A into integers, using a map to store the values is very useful:
;; (get {\A 100, \B 20} \B) ;=> 20
;; ({\A 100, \B 20} \B) ;=> 20
;; (def replacements {\A 100, \B 20})
;; (replacements \B) ;=> 20
;;

;; Exercise 2
;; Write the function (rank card) which takes a single card and returns the rank as a number between 2 and 14.
;;
;; string -> integer
;; Get first element, assess digit or character, return as digit if digit, convert to digit if chracter
(defn rank
  "Convert character rank to integer rank."
  [card]  
  (let [c-rank      (first card)
        replace-map {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    ;;
    (if (Character/isDigit c-rank)
      (Integer/valueOf (str c-rank))
      (get replace-map c-rank))))
;;
(rank "2H") ;=> 2
(rank "4S") ;=> 4
(rank "TS") ;=> 10
(rank "JS") ;=> 11
(rank "QS") ;=> 12
(rank "KS") ;=> 13
(rank "AS") ;=> 14


;; Exercise 1
;; Write the function (suit card) which takes a singe card and returns the suit of the card as a one character string.
;;
;; string -> string
;; Get the second letter
(defn suit [card]
  (str (second card)))
(suit "2H") ;=> "H"
(suit "2D") ;=> "D"
(suit "2C") ;=> "C"
(suit "3S") ;=> "S"


;; hands
(def high-seven                   ["2H" "3S" "4C" "5C" "7D"])
(def pair-hand                    ["2H" "2S" "4C" "5C" "7D"])
(def two-pairs-hand               ["2H" "2S" "4C" "4D" "7D"])
(def three-of-a-kind-hand         ["2H" "2S" "2C" "4D" "7D"])
(def four-of-a-kind-hand          ["2H" "2S" "2C" "2D" "7D"])
(def straight-hand                ["2H" "3S" "6C" "5D" "4D"])
(def low-ace-straight-hand        ["2H" "3S" "4C" "5D" "AD"])
(def high-ace-straight-hand       ["TH" "AS" "QC" "KD" "JD"])
(def flush-hand                   ["2H" "4H" "5H" "9H" "7H"])
(def full-house-hand              ["2H" "5D" "2D" "2C" "5S"])
(def straight-flush-hand          ["2H" "3H" "6H" "5H" "4H"])
(def low-ace-straight-flush-hand  ["2D" "3D" "4D" "5D" "AD"])
(def high-ace-straight-flush-hand ["TS" "AS" "QS" "KS" "JS"])


;; Exercise 3
;; Write the function (pair? hand) that returns true if there is a pair in hand and false if there is no pair in hand.
;;
;; vector of strings -> bool
;; check rank of each card, and return true if duplications
(defn pair? [hand]
  ;; Create a frequency map of ranks
  (let [freq-map (frequencies (map rank hand))]
    ;; Check max, if 2+, then true, otherwise false.
    (>= (reduce max (vals freq-map)) 2)))
;;
(pair? pair-hand)  ;=> true
(pair? high-seven) ;=> false


;; Exercise 4
;; Write the function (three-of-a-kind? hand) that returns true if the hand contains a three of a kind.
;;
;; vector of strings -> bool
;; Check if there are 3 card of the same ranks
(defn three-of-a-kind? [hand]
  ;; Create a frequency map of ranks
  (let [freq-map (frequencies (map rank hand))]
    ;; Check max, if 3+, then true, otherwise false.
    (>= (reduce max (vals freq-map)) 3)))
(three-of-a-kind? two-pairs-hand)       ;=> false
(three-of-a-kind? three-of-a-kind-hand) ;=> true


;; Exercise 5
;; Write the function (four-of-a-kind? hand) that returns true if the hand contains a four of a kind.
;;
(defn four-of-a-kind? [hand]
  ;; Create a frequency map of ranks
  (let [freq-map (frequencies (map rank hand))]
    ;; Check max, if 4+, then true, otherwise false.
    (>= (reduce max (vals freq-map)) 4)))
;;
(four-of-a-kind? two-pairs-hand)      ;=> false
(four-of-a-kind? four-of-a-kind-hand) ;=> true


;; Exercise 6
;; Write the function (flush? hand) that returns true if the hand is a flush.
;;
;; vector of strings -> bool
;; Check if all five cards are of the same suit
(defn flush? [hand]
  ;; if all suits are equal that is flush
  (apply = (map suit hand)))
;;
(flush? pair-hand)  ;=> false
(flush? flush-hand) ;=> true)


;; Exercise 7
;; Write the function (full-house? hand) that returns true if hand is a full house, and otherwise false.
;; vector of strings -> bool
;; Check if a pair and 3-of-kind
(defn full-house? [hand]
  ;; Create a frequency map of ranks
  (let [freq-map (frequencies (map rank hand))]
    ;; If a pair and 3 of kind, then true, otherwise false
    (= #{2 3} (set (vals freq-map)))))
;;
(full-house? three-of-a-kind-hand) ;=> false
(full-house? full-house-hand)      ;=> true


;; Exercise 8
;; Write the function (two-pairs? hand) that return true if hand has two pairs, and otherwise false.
;; Note that a four of a kind is also two pairs.
;;
;; vector of strings -> bool
;; Check if 2 pairs
(defn two-pairs? [hand]
  ;; Create a frequency map of ranks
  (let [freq-map (frequencies (map rank hand))]
    (or
     ;; strictly two paris
     (= [1 2 2] (sort (vals freq-map)))
     ;; four-of-a-kind is two parirs
     (four-of-a-kind? hand))))
;;
(two-pairs? two-pairs-hand)      ;=> true
(two-pairs? pair-hand)           ;=> false
(two-pairs? four-of-a-kind-hand) ;=> true


;; Exercise 9
;; Write the function (straight? hand) that returns true if hand is a straight, and otherwise false.
;; Note that an ace is accepted both as a rank 1 and rank 14 card in straights.
;;
;; vector of strings -> bool
(defn straight? [hand]
  (let [;; Ace as 14
        sorted-hand-a-as-14 (sort (map rank hand))
        min-14              (first sorted-hand-a-as-14)
        correspond-seq-14   (range  min-14 (+ 5 min-14))
        
        ;; Ace as 1
        sorted-hand-a-as-1  (sort (replace {14 1} sorted-hand-a-as-14))
        min-1               (first sorted-hand-a-as-1)
        correspond-seq-1    (range  min-1 (+ 5 min-1))]
    ;;
    (or
     (= sorted-hand-a-as-1  correspond-seq-1)
     (= sorted-hand-a-as-14 correspond-seq-14))))
;;
(straight? two-pairs-hand)             ;=> false
(straight? straight-hand)              ;=> true
(straight? low-ace-straight-hand)      ;=> true
(straight? ["2H" "2D" "3H" "4H" "5H"]) ;=> false
(straight? high-ace-straight-hand)     ;=> true


;; Exercise 10
;; Write the function (straight-flush? hand) which returns true if the hand is a straight flush, that is both a straight and a flush, and otherwise false.
;;
;; vector of strings -> bool
;; Check flush? AND straight?
(defn straight-flush? [hand]
  (and
   (flush?    hand)
   (straight? hand)))
;;
(straight-flush? straight-hand)                ;=> false
(straight-flush? flush-hand)                   ;=> false
(straight-flush? straight-flush-hand)          ;=> true
(straight-flush? low-ace-straight-flush-hand)  ;=> true
(straight-flush? high-ace-straight-flush-hand) ;=> true



;; Exercise 11
;; Write the function (value hand), which returns the value of a hand according to the table above.
;;
;; vector of strings -> integer
;; Assess hand for score
;; implementation with cond
(defn value [hand]
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
;;
(value high-seven)           ;=> 0
(value pair-hand)            ;=> 1
(value two-pairs-hand)       ;=> 2
(value three-of-a-kind-hand) ;=> 3
(value straight-hand)        ;=> 4
(value flush-hand)           ;=> 5
(value full-house-hand)      ;=> 6
(value four-of-a-kind-hand)  ;=> 7
(value straight-flush-hand)  ;=> 8
;;
;; The example below checks for all patters, cond will short-circuit, and is more efficient.
;;
;; It might be helpful to add a checker (high-card? hand):
(defn high-card? [hand]
  true) ; All hands have a high card.
;; You can create a sequence of [matcher value] pairs like so:
;; (let [checkers #{[high-card? 0]  [pair? 1]
;;                  [two-pairs? 2]  [three-of-a-kind? 3]
;;                  [straight? 4]   [flush? 5]
;;                  [full-house? 6] [four-of-a-kind? 7]
;;                  [straight-flush? 8]}]
;;   ...)
;; You can now use filter, map and apply max to get the highest value that a hand has. The function second can be useful. Remember to use let to give the intermediate results readable names.
(second [:i :am :a :sequence]) ;=> :am
(second [two-pairs? 2])        ;=> 2
