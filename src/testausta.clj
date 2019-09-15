(use 'p-p-p-pokerface)

(suit "AH")
(rank "2H")

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

(pair? pair-hand)  ;=> true
(pair? high-seven) ;=> false
(three-of-a-kind? two-pairs-hand)       ;=> false
(three-of-a-kind? three-of-a-kind-hand) ;=> true
(four-of-a-kind? two-pairs-hand)      ;=> false
(four-of-a-kind? four-of-a-kind-hand) ;=> true
(flush? pair-hand)  ;=> false
(flush? flush-hand) ;=> true
(full-house? three-of-a-kind-hand) ;=> false
(full-house? full-house-hand)      ;=> true
(two-pairs? two-pairs-hand)      ;=> true
(two-pairs? pair-hand)           ;=> false
(two-pairs? four-of-a-kind-hand) ;=> true


(def sorted-hand (sort (map rank straight-hand)))

(range (first sorted-hand) (+ (first sorted-hand) 5))

(straight? two-pairs-hand)             ;=> false
(straight? straight-hand)              ;=> true
(straight? low-ace-straight-hand)      ;=> true
(straight? ["2H" "2D" "3H" "4H" "5H"]) ;=> false
(straight? high-ace-straight-hand)     ;=> true

(straight-flush? straight-hand)                ;=> false
(straight-flush? flush-hand)                   ;=> false
(straight-flush? straight-flush-hand)          ;=> true
(straight-flush? low-ace-straight-flush-hand)  ;=> true
(straight-flush? high-ace-straight-flush-hand) ;=> true

(value high-seven)           ;=> 0
(value pair-hand)            ;=> 1
(value two-pairs-hand)       ;=> 2
(value three-of-a-kind-hand) ;=> 3
(value straight-hand)        ;=> 4
(value flush-hand)           ;=> 5
(value full-house-hand)      ;=> 6
(value four-of-a-kind-hand)  ;=> 7
(value straight-flush-hand)  ;=> 8

(defn newvalue [hand]
    (let [
          high-card? (fn [x] true)
          checkers #{[high-card? 0] [pair? 1] [two-pairs? 2] [three-of-a-kind? 3]
                    [straight? 4] [flush? 5] [full-house? 6] [four-of-a-kind? 7]
                    [straight-flush? 8]
                    }
          check-hand (fn [f] ((first f) hand))
          hand-value (fn [f] (second f))
         ]
    (apply max (map hand-value (filter check-hand checkers)))))

(newvalue high-seven)
(newvalue pair-hand)            ;=> 1
(newvalue two-pairs-hand)       ;=> 2
(newvalue three-of-a-kind-hand) ;=> 3
(newvalue straight-hand)        ;=> 4
(newvalue flush-hand)           ;=> 5
(newvalue full-house-hand)      ;=> 6
(newvalue four-of-a-kind-hand)  ;=> 7
(newvalue straight-flush-hand)  ;=> 8





