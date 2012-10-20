(ns p-p-p-pokerface.core-test
  (:use midje.sweet
        p-p-p-pokerface.core))

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
  (pair? pair-hand) => truthy
  (pair? high-seven) => falsey)

(facts "two-pairs?"
  (two-pairs? two-pairs-hand) => truthy
  (two-pairs? pair-hand) => falsey
  (two-pairs? four-of-a-kind-hand) => truthy)

(facts "three-of-a-kind?"
  (three-of-a-kind? two-pairs-hand) => falsey
  (three-of-a-kind? three-of-a-kind-hand) => truthy)

(facts "four-of-a-kind?"
  (four-of-a-kind? two-pairs-hand) => falsey
  (four-of-a-kind? four-of-a-kind-hand) => truthy)

(facts "straight?"
  (straight? two-pairs-hand) => falsey
  (straight? straight-hand) => truthy
  (straight? low-ace-straight-hand) => truthy
  (straight? ["2H" "2D" "3H" "4H" "5H"]) => falsey
  (straight? high-ace-straight-hand) => truthy)

(facts "flush?"
  (flush? pair-hand) => falsey
  (flush? flush-hand) => truthy)

(facts "full-house?"
  (full-house? three-of-a-kind-hand) => falsey
  (full-house? full-house-hand) => truthy)

(facts "straight-flush?"
  (straight-flush? straight-hand) => falsey
  (straight-flush? flush-hand) => falsey
  (straight-flush? straight-flush-hand) => truthy
  (straight-flush? low-ace-straight-flush-hand) => truthy
  (straight-flush? high-ace-straight-flush-hand) => truthy)

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