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
    ;; Check max, if greater than 1, then true, otherwise false.
    (> (reduce max (vals freq-map)) 1)))
;;
(pair? pair-hand)  ;=> true
(pair? high-seven) ;=> false


(defn three-of-a-kind? [hand]
  nil)

(defn four-of-a-kind? [hand]
  nil)

(defn flush? [hand]
  nil)

(defn full-house? [hand]
  nil)

(defn two-pairs? [hand]
  nil)

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
