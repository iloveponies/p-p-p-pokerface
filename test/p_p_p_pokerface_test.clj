(ns p-p-p-pokerface-test
  (:use iloveponies.tests.p-p-p-pokerface)
  (:use p-p-p-pokerface))
; Exercise 1
(suit "2H") ;=> "H"
(suit "2D") ;=> "D"
(suit "2C") ;=> "C"
(suit "3S") ;=> "S"

; Exercise 2
(rank "2H") ;=> 2
(rank "4S") ;=> 4
(rank "TS") ;=> 10
(rank "JS") ;=> 11
(rank "QS") ;=> 12
(rank "KS") ;=> 13
(rank "AS") ;=> 14

(get card-map "2")
(get card-map "K")

(let [[rank _] "2H"]
  #_(get card-map rank)
  rank)


(comment
  (def high-seven ["2H" "3S" "4C" "5C" "7D"])
  (def pair-hand ["2H" "2S" "4C" "5C" "7D"])
  (def two-pairs-hand ["2H" "2S" "4C" "4D" "7D"])
  (def three-of-a-kind-hand ["2H" "2S" "2C" "4D" "7D"])
  (def four-of-a-kind-hand ["2H" "2S" "2C" "2D" "7D"])
  (def straight-hand ["2H" "3S" "6C" "5D" "4D"])
  (def low-ace-straight-hand ["2H" "3S" "4C" "5D" "AD"])
  (def high-ace-straight-hand ["TH" "AS" "QC" "KD" "JD"])
  (def flush-hand ["2H" "4H" "5H" "9H" "7H"])
  (def full-house-hand ["2H" "5D" "2D" "2C" "5S"])
  (def straight-flush-hand ["2H" "3H" "6H" "5H" "4H"])
  (def low-ace-straight-flush-hand ["2D" "3D" "4D" "5D" "AD"])
  (def high-ace-straight-flush-hand ["TS" "AS" "QS" "KS" "JS"]))

#_(pair? pair-hand)  ;=> true
#_(pair? high-seven) ;=> false
