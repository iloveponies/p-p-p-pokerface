(ns p-p-p-pokerface-test
  (:use midje.sweet
        p-p-p-pokerface))

(facts "suit" {:exercise 1
               :points 1}
  (suit "2H") => "H"
  (suit "2D") => "D"
  (suit "2C") => "C"
  (suit "3S") => "S")

(facts "rank" {:exercise 2
               :points 1}
  (rank "2H") => 2
  (rank "4S") => 4
  (rank "TS") => 10
  (rank "JS") => 11
  (rank "QS") => 12
  (rank "KS") => 13
  (rank "AS") => 14)

(def high-seven ["2H" "3S" "4C" "5C" "7D"])

(def pair-hand ["2H" "2S" "4C" "5C" "7D"])

(def pair-hands #{["2H" "2S" "4C" "5C" "7D"]
                  ["2S" "4S" "4C" "9D" "KS"]})

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

(facts "pair?" {:exercise 3
                :points 1}
  (every? pair? pair-hands) => true
  (pair? high-seven) => false)

(facts "three-of-a-kind?" {:exercise 4
                           :points 1}
  (three-of-a-kind? two-pairs-hand) => false
  (three-of-a-kind? three-of-a-kind-hand) => true)

(facts "four-of-a-kind?" {:exercise 5
                          :points 1}
  (four-of-a-kind? two-pairs-hand) => false
  (four-of-a-kind? four-of-a-kind-hand) => true)

(facts "flush?" {:exercise 6
                 :points 1}
  (flush? pair-hand) => false
  (flush? flush-hand) => true)

(facts "full-house?" {:exercise 7
                      :points 1}
  (full-house? three-of-a-kind-hand) => false
  (full-house? four-of-a-kind-hand) => false
  (full-house? full-house-hand) => true)

(facts "two-pairs?" {:exercise 8
                     :points 1}
  (two-pairs? two-pairs-hand) => true
  (two-pairs? three-of-a-kind-hand) => false
  (two-pairs? pair-hand) => false)

(tabular
 (facts "straight?" {:exercise 9
                     :points 1}
   (straight? ?hand) => ?result)
 ?hand                      ?result
 two-pairs-hand             false
 straight-hand              true
 low-ace-straight-hand      true
 ["2H" "2D" "3H" "4H" "5H"] false
 ["2H" "3H" "3D" "4H" "6H"] false
 high-ace-straight-hand     true)

(facts "straight-flush?" {:exercise 10
                          :points 1}
  (straight-flush? straight-hand) => false
  (straight-flush? flush-hand) => false
  (straight-flush? straight-flush-hand) => true
  (straight-flush? low-ace-straight-flush-hand) => true
  (straight-flush? high-ace-straight-flush-hand) => true)

(facts "value" {:exercise 11
                :points 1}
  (value high-seven) => 0
  (every? (partial = 1) (map value pair-hands)) => truthy
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
