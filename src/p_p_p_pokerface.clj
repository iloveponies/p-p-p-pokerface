(ns p-p-p-pokerface)

(def rank-character {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[fst snd] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (rank-character fst))))

(rank "2H") ;=> 2
(rank "4S") ;=> 4
(rank "TS") ;=> 10
(rank "JS") ;=> 11
(rank "QS") ;=> 12
(rank "KS") ;=> 13
(rank "AS") ;=> 14

(defn suit [card]
  (let [[fst snd] card]
    (str snd)))

(suit "2H") ;=> "H"
(suit "2D") ;=> "D"
(suit "2C") ;=> "C"
(suit "3S") ;=> "S"

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

(defn pair? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (= (apply max freqs) 2)))

;; (pair? pair-hand)  ;=> true
;; (pair? high-seven) ;=> false

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (= (apply max freqs) 3)))

;; (three-of-a-kind? two-pairs-hand)       ;=> false
;; (three-of-a-kind? three-of-a-kind-hand) ;=> true

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (= (apply max freqs) 4)))

;; (four-of-a-kind? two-pairs-hand)      ;=> false
;; (four-of-a-kind? four-of-a-kind-hand) ;=> true

(defn flush? [hand]
  (let [suits (map suit hand)
        freqs (vals (frequencies suits))]
    (= (apply max freqs) 5)))

;; (flush? pair-hand)  ;=> false
;; (flush? flush-hand) ;=> true

(defn full-house? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (and 
     (= (apply max freqs) 3)
     (= (apply min freqs) 2))))

;; (full-house? three-of-a-kind-hand) ;=> false
;; (full-house? full-house-hand)      ;=> true

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (or
     (four-of-a-kind? hand)
     (and
      (= (apply max freqs) 2)
      (= (count freqs) 3)))))

;; (two-pairs? two-pairs-hand)      ;=> true
;; (two-pairs? pair-hand)           ;=> false
;; (two-pairs? four-of-a-kind-hand) ;=> true

(defn straight? [hand]
  (let [ranks (sort (replace rank-character (map rank hand)))
        first (first ranks)
        expect (range first (+ first 5))
        royal [2 3 4 5 14]]
    (or 
     (= royal ranks)
     (= expect ranks))))

;; (straight? two-pairs-hand)             ;=> false
;; (straight? straight-hand)              ;=> true
;; (straight? low-ace-straight-hand)      ;=> true
;; (straight? ["2H" "2D" "3H" "4H" "5H"]) ;=> false
;; (straight? high-ace-straight-hand)     ;=> true
    
(defn straight-flush? [hand]
  (and
   (straight? hand)
   (flush? hand)))

;; (straight-flush? straight-hand)                ;=> false
;; (straight-flush? flush-hand)                   ;=> false
;; (straight-flush? straight-flush-hand)          ;=> true
;; (straight-flush? low-ace-straight-flush-hand)  ;=> true
;; (straight-flush? high-ace-straight-flush-hand) ;=> true

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (map second (filter (fn [checker]
                                     ((first checker) hand))
                                   checkers)))))

;; (value high-seven)           ;=> 0
;; (value pair-hand)            ;=> 1
;; (value two-pairs-hand)       ;=> 2
;; (value three-of-a-kind-hand) ;=> 3
;; (value straight-hand)        ;=> 4
;; (value flush-hand)           ;=> 5
;; (value full-house-hand)      ;=> 6
;; (value four-of-a-kind-hand)  ;=> 7
;; (value straight-flush-hand)  ;=> 8
