(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card
        high-value-map {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (get high-value-map fst))))

;(rank "4S") ;=> 4
;(rank "TS") ;=> 10
;(rank "JS") ;=> 11
;(rank "QS") ;=> 12
;(rank "KS") ;=> 13
;(rank "AS") ;=> 14

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

;(suit "2H") ;=> "H"
;(suit "2D") ;=> "D"
;(suit "2C") ;=> "C"
;(suit "3S") ;=> "S"

(defn x-of-a-kind-by-attribute [hand attribute x]
  (contains? (set (vals (frequencies (map attribute hand)))) x))

(defn x-of-a-kind-by-rank [hand x]
  (x-of-a-kind-by-attribute hand rank x))

(defn x-of-a-kind-by-suit [hand x]
  (x-of-a-kind-by-attribute hand suit x))

(defn pair? [hand]
  (x-of-a-kind-by-rank hand 2))

;(pair? pair-hand)  ;=> true
;(pair? high-seven) ;=> false

(defn three-of-a-kind? [hand]
  (x-of-a-kind-by-rank hand 3))

;(three-of-a-kind? two-pairs-hand)       ;=> false
;(three-of-a-kind? three-of-a-kind-hand) ;=> true

(defn four-of-a-kind? [hand]
  (x-of-a-kind-by-rank hand 4))

;(four-of-a-kind? two-pairs-hand)      ;=> false
;(four-of-a-kind? four-of-a-kind-hand) ;=> true

(defn flush? [hand]
  (x-of-a-kind-by-suit hand 5))

;(flush? pair-hand)  ;=> false
;(flush? flush-hand) ;=> true)

(defn full-house? [hand]
  (and (x-of-a-kind-by-rank hand 2) (x-of-a-kind-by-rank hand 3)))

;(full-house? three-of-a-kind-hand) ;=> false
;(full-house? full-house-hand)      ;=> true

(defn two-pairs? [hand]
  (let [fs (vals (frequencies (map rank hand)))
        pick-x (fn [xs n] (filter (fn [x] (= n x)) xs))
        pick-two (fn [xs] (pick-x xs 2))
        pick-four (fn [xs] (pick-x xs 4))]
      (or (= 2 (count (pick-two fs))) (= 1 (count (pick-four fs))))
    ))

;(two-pairs? two-pairs-hand)      ;=> true
;(two-pairs? pair-hand)           ;=> false
;(two-pairs? four-of-a-kind-hand) ;=> true

(defn straight? [hand]
  (let [ranks (map rank hand)
        lo-sorted-ranks (sort (replace {14 1} ranks))
        hi-sorted-ranks (sort (replace {1 14} ranks))
        straight (fn [rs] (range (first rs) (+ 5 (first rs))))
        straight= (fn [xs] (= xs (straight xs)))
        ]
    (or (straight= lo-sorted-ranks) (straight= hi-sorted-ranks))
    ))

;(sort (replace {14 1}  (map rank low-ace-straight-hand)))
;(sort (map rank low-ace-straight-hand))
;(map rank low-ace-straight-hand)

;(straight? two-pairs-hand)             ;=> false
;(straight? straight-hand)              ;=> true
;(straight? low-ace-straight-hand)      ;=> true
;(straight? ["2H" "2D" "3H" "4H" "5H"]) ;=> false
;(straight? high-ace-straight-hand)     ;=> true

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

;(def high-seven                   ["2H" "3S" "4C" "5C" "7D"])
;(def pair-hand                    ["2H" "2S" "4C" "5C" "7D"])
;(def two-pairs-hand               ["2H" "2S" "4C" "4D" "7D"])
;(def three-of-a-kind-hand         ["2H" "2S" "2C" "4D" "7D"])
;(def four-of-a-kind-hand          ["2H" "2S" "2C" "2D" "7D"])
;(def straight-hand                ["2H" "3S" "6C" "5D" "4D"])
;(def low-ace-straight-hand        ["2H" "3S" "4C" "5D" "AD"])
;(def high-ace-straight-hand       ["TH" "AS" "QC" "KD" "JD"])
;(def flush-hand                   ["2H" "4H" "5H" "9H" "7H"])
;(def full-house-hand              ["2H" "5D" "2D" "2C" "5S"])
;(def straight-flush-hand          ["2H" "3H" "6H" "5H" "4H"])
;(def low-ace-straight-flush-hand  ["2D" "3D" "4D" "5D" "AD"])
;(def high-ace-straight-flush-hand ["TS" "AS" "QS" "KS" "JS"])

;(straight-flush? straight-hand)                ;=> false
;(straight-flush? flush-hand)                   ;=> false
;(straight-flush? straight-flush-hand)          ;=> true
;(straight-flush? low-ace-straight-flush-hand)  ;=> true
;(straight-flush? high-ace-straight-flush-hand) ;=> true

(defn high-card? [hand] true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]

    (apply max (map (fn [[_ v]] v) (filter (fn [[checker value]] (checker hand) ) checkers)))
  ))


;(value high-seven)           ;=> 0
;(value pair-hand)            ;=> 1
;(value two-pairs-hand)       ;=> 2
;(value three-of-a-kind-hand) ;=> 3
;(value straight-hand)        ;=> 4
;(value flush-hand)           ;=> 5
;(value full-house-hand)      ;=> 6
;(value four-of-a-kind-hand)  ;=> 7
;(value straight-flush-hand)  ;=> 8
