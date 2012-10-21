(ns p-p-p-pokerface-test
  (:use midje.sweet
        p-p-p-pokerface))

(facts "rank"
  (rank "2H") => 2
  (rank "4S") => 4
  (rank "TS") => 10
  (rank "JS") => 11
  (rank "QS") => 12
  (rank "KS") => 13
  (rank "AS") => 14)

(facts "suit"
  (suit "2H") => "H"
  (suit "2D") => "D"
  (suit "2C") => "C"
  (suit "3S") => "S")

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
(def high-ace-straight-flush-hand ["TS" "AS" "QS" "KS" "JS"])

(facts "pair"
  (pair? pair-hand) => true
  (pair? high-seven) => false)

(facts "two-pairs?"
  (two-pairs? two-pairs-hand) => true
  (two-pairs? pair-hand) => false
  (two-pairs? four-of-a-kind-hand) => true)

(facts "three-of-a-kind?"
  (three-of-a-kind? two-pairs-hand) => false
  (three-of-a-kind? three-of-a-kind-hand) => true)

(facts "four-of-a-kind?"
  (four-of-a-kind? two-pairs-hand) => false
  (four-of-a-kind? four-of-a-kind-hand) => true)

(facts "straight?"
  (straight? two-pairs-hand) => false
  (straight? straight-hand) => true
  (straight? low-ace-straight-hand) => true
  (straight? ["2H" "2D" "3H" "4H" "5H"]) => false
  (straight? high-ace-straight-hand) => true)

(facts "flush?"
  (flush? pair-hand) => false
  (flush? flush-hand) => true)

(facts "full-house?"
  (full-house? three-of-a-kind-hand) => false
  (full-house? full-house-hand) => true)

(facts "straight-flush?"
  (straight-flush? straight-hand) => false
  (straight-flush? flush-hand) => false
  (straight-flush? straight-flush-hand) => true
  (straight-flush? low-ace-straight-flush-hand) => true
  (straight-flush? high-ace-straight-flush-hand) => true)

(facts "value"
  (value high-seven) => 0
  (value pair-hand) => 1
  (value two-pairs-hand) => 2
  (value three-of-a-kind-hand) => 3
  (value straight-hand) => 4
  (value flush-hand) => 5
  (value full-house-hand) => 6
  (value four-of-a-kind-hand) => 7
  (value straight-flush-hand) => 8)

(let [hand-has-value? (resolve 'hand-has-value?)]
  (if (and hand-has-value?
           (bound? hand-has-value?))
    (facts "hand-has-value?"
           (hand-has-value? pair-hand 1) => true
           (hand-has-value? full-house-hand 6) => true
           (hand-has-value? full-house-hand 3) => true
           (hand-has-value? three-of-a-kind-hand 3) => true
           (hand-has-value? three-of-a-kind-hand 6) => false)))

(let [hand-has-type? (resolve 'hand-has-type?)]
  (if (and hand-has-type?
           (bound? hand-has-type?))
    (facts "hand-has-type?"
           (hand-has-type? pair-hand [pair? 1]) => true
           (hand-has-type? pair-hand [two-pairs? 2]) => false
           (hand-has-type? full-house-hand [three-of-a-kind? 3]) => true
           (hand-has-type? full-house-hand [full-house? 6]) => true
           (hand-has-type? full-house-hand [flush? 5]) => false)))