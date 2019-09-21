(ns p-p-p-pokerface)

(defn rank
  "Returns the rank as an int
  between 2 and 14"
  [card]
  (let [transform {\T 10
                   \J 11
                   \Q 12
                   \K 13
                   \A 14}
        rank (first card)]
    (if (Character/isDigit rank)
      (Integer. (str rank))
      (transform rank))))

;; (rank "2H") ;=> 2
;; (rank "4S") ;=> 4
;; (rank "TS") ;=> 10
;; (rank "JS") ;=> 11
;; (rank "QS") ;=> 12
;; (rank "KS") ;=> 13
;; (rank "AS") ;=> 14

(defn suit
  "Returns suit of a card,
  assumes card is two-char string"
  [card]
  (str (last card)))

;; (suit "2H") ;=> "H"
;; (suit "2D") ;=> "D"
;; (suit "2C") ;=> "C"
;; (suit "3S") ;=> "S"

(frequencies [4 4 7 7 4])
(keys (frequencies [4 4 7 7 4]))
(vals (frequencies [4 4 7 7 4]))

(max [1 2])
(max 1 2)

;; (def high-seven                   ["2H" "3S" "4C" "5C" "7D"])
;; (def pair-hand                    ["2H" "2S" "4C" "5C" "7D"])
;; (def two-pairs-hand               ["2H" "2S" "4C" "4D" "7D"])
;; (def three-of-a-kind-hand         ["2H" "2S" "2C" "4D" "7D"])
;; (def four-of-a-kind-hand          ["2H" "2S" "2C" "2D" "7D"])
;; (def straight-hand                ["2H" "3S" "6C" "5D" "4D"])
;; (def low-ace-straight-hand        ["2H" "3S" "4C" "5D" "AD"])
;; (def high-ace-straight-hand       ["TH" "AS" "QC" "KD" "JD"])
;; (def flush-hand                   ["2H" "4H" "5H" "9H" "7H"])
;; (def full-house-hand              ["2H" "5D" "2D" "2C" "5S"])
;; (def straight-flush-hand          ["2H" "3H" "6H" "5H" "4H"])
;; (def low-ace-straight-flush-hand  ["2D" "3D" "4D" "5D" "AD"])
;; (def high-ace-straight-flush-hand ["TS" "AS" "QS" "KS" "JS"])

(defn pair?
  "Takes in a vector representing a hand
  returns true if a pair exists"
  [hand]
  (let [ranks_freq (vals (frequencies (map rank hand)))]
    (if (some #(== % 2) ranks_freq)
      true
      false)))

;; (pair? pair-hand)  ;=> true
;; (pair? high-seven) ;=> false

(defn three-of-a-kind? [hand]
  (let [ranks_freq (vals (frequencies (map rank hand)))]
    (if (some #(== % 3) ranks_freq)
      true
      false)))

;; (three-of-a-kind? two-pairs-hand)       ;=> false
;; (three-of-a-kind? three-of-a-kind-hand) ;=> true

(defn four-of-a-kind? [hand]
  (let [ranks_freq (vals (frequencies (map rank hand)))]
    (if (some #(== % 4) ranks_freq)
      true
      false)))

;; (four-of-a-kind? two-pairs-hand)      ;=> false
;; (four-of-a-kind? four-of-a-kind-hand) ;=> true

(defn flush? [hand]
  (let [suit-map (frequencies (map suit hand))]
    (= 1 (count suit-map))))

;; (flush? pair-hand)  ;=> false
;; (flush? flush-hand) ;=> true

(defn full-house? [hand]
  (and (three-of-a-kind? hand)
       (pair? hand)))

;; (full-house? three-of-a-kind-hand) ;=> false
;; (full-house? full-house-hand)      ;=> true

(defn two-pairs?
  "Looks at frequencies to determine if
  hand is a two-pair"

  [hand]
  (let [rank-freq (vals (frequencies (map rank hand)))]
    (cond
       (four-of-a-kind? hand) true
       (= '(2 2 1) rank-freq) true
       :else false)))

;; (two-pairs? two-pairs-hand)      ;=> true
;; (two-pairs? pair-hand)           ;=> false
;; (two-pairs? four-of-a-kind-hand) ;=> true

(defn straight? [hand]
  (let [ace->1 {14 1}
        sorted-ranks (sort (keys (frequencies (map rank hand))))
        five-ranks (= 5 (count sorted-ranks))
        first-card (first sorted-ranks)]
    (cond
       (and five-ranks
            (= sorted-ranks
               (range first-card (+ 5 first-card)))) true
       (and five-ranks
            (= (sort (replace ace->1 sorted-ranks)) ;; I sort again, I'm a monster
               (range 1 6))) true
       :else false)))

;; (straight? two-pairs-hand)             ;=> false
;; (straight? straight-hand)              ;=> true
;; (straight? low-ace-straight-hand)      ;=> true
;; (straight? ["2H" "2D" "3H" "4H" "5H"]) ;=> false
;; (straight? high-ace-straight-hand)     ;=> true

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

;; (straight-flush? straight-hand)                ;=> false
;; (straight-flush? flush-hand)                   ;=> false
;; (straight-flush? straight-flush-hand)          ;=> true
;; (straight-flush? low-ace-straight-flush-hand)  ;=> true
;; (straight-flush? high-ace-straight-flush-hand) ;=> true

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn value [hand]
  (let [checkers #{[high-card? 0]
                   [pair? 1]
                   [two-pairs? 2]
                   [three-of-a-kind? 3]
                   [straight? 4]
                   [flush? 5]
                   [full-house? 6]
                   [four-of-a-kind? 7]
                   [straight-flush? 8]}
        check-fn (fn [checker]
                   (if ((first checker) hand)
                     (second checker)
                     0))]
    (apply max (map check-fn checkers))))

;; (value high-seven)           ;=> 0
;; (value pair-hand)            ;=> 1
;; (value two-pairs-hand)       ;=> 2
;; (value three-of-a-kind-hand) ;=> 3
;; (value straight-hand)        ;=> 4
;; (value flush-hand)           ;=> 5
;; (value full-house-hand)      ;=> 6
;; (value four-of-a-kind-hand)  ;=> 7
;; (value straight-flush-hand)  ;=> 8
