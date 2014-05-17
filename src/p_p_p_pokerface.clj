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
(defn rank [card]
  (let [c-rank (first card)]
    (if (Character/isDigit c-rank)
      (Integer/valueOf (str c-rank))
      (get {\T 10, \J 11, \Q 12, \K 13, \A 14} c-rank))))
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


(defn pair? [hand]
  nil)

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
