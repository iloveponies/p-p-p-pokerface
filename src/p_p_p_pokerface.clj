(ns p-p-p-pokerface)

(defn char->number
  "return false if not a number"
  [char]
    (if (Character/isDigit char)
      (Integer/valueOf (str char))
      (str char)))


(defn rank [card]
 (let [  [char-card]   card
         value-card  (char->number char-card)]
  (if (Character/isDigit char-card)
    value-card
    (cond
      (= "T" value-card) 10
      (= "J" value-card) 11
      (= "Q" value-card) 12
      (= "K" value-card) 13
      (= "A" value-card) 14
      :else nil))))


;; (rank "2H") ;=> 2
;; (rank "4S") ;=> 4
;; (rank "TS") ;=> 10
;; (rank "JS") ;=> 11
;; (rank "QS") ;=> 12
;; (rank "KS") ;=> 13
;; (rank "AS") ;=> 14



(defn suit [card]
  (str (get card 1)))


;; (suit "2H") ;=> "H"
;; (suit "2D") ;=> "D"
;; (suit "2C") ;=> "C"
;; (suit "3S") ;=> "S"

(defn number-ranks [hand]
  (vals(frequencies (map #(rank % ) hand))))

;;(number-ranks pair-hand)

(defn  n-pair>1 [n]
 "return function : return the number of n pair "
(fn [hand]
  (<= 1 (count (filter #(= n %) (number-ranks hand))))))


(defn pair? [hand]
  " true if exist  one pair "
  (let [ n-pair? (n-pair>1 2)]
    (n-pair? hand)))

;; (pair? pair-hand)  ;=> true
;; (pair? high-seven) ;=> false

(defn three-of-a-kind? [hand]
  " true if exist a 3-pairs "
  (let [ n-pair? (n-pair>1 3)]
    (n-pair? hand)))

;; (three-of-a-kind? two-pairs-hand)       ;=> false
;; (three-of-a-kind? three-of-a-kind-hand) ;=> true

(defn four-of-a-kind? [hand]
  " true if axist a four-pairs "
  (let [ n-pair? (n-pair>1 4)]
    (n-pair? hand)))

;; (four-of-a-kind? two-pairs-hand)      ;=> false
;; (four-of-a-kind? four-of-a-kind-hand) ;=> true


(defn number-type-suit [hand]
  (count (frequencies (map #(suit %) hand))))

(defn flush? [hand]
   (= 1 (number-type-suit hand)))

;; (flush? pair-hand)  ;=> false
;; (flush? flush-hand) ;=> true


(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

;; (full-house? three-of-a-kind-hand) ;=> false
;; (full-house? full-house-hand)      ;=> true



(defn number-two-pairs  [hand]
  (count (filter #(= 2 %) (number-ranks hand))))

(defn two-pairs? [hand]
  "return true if hand contains two pairs"
  (= 2 (number-two-pairs hand)))

;; (two-pairs? two-pairs-hand)      ;=> true
;; (two-pairs? pair-hand)           ;=> false
;; (two-pairs? four-of-a-kind-hand) ;=> true

(defn hand-ranks [hand]
  "return a sorted vector [rank of each card] "
  (sort (map #(rank %) hand)))

;;(hand-ranks ["2H" "2S" "4C" "5C" "7D"]) ;=>(2 2 4 5 7)

(defn successor? [liste]
  "return true if liste = (range (min liste)   (max liste))"
  (let [min-liste (apply min liste)
        max-liste (apply max liste)]
        (= (range min-liste (inc max-liste)) (sort liste))))

;; (liste-successor? [2 3 4 5 14])   ;;false
;; (liste-successor? [2 3 4 5 6])   ;;true

;; (defn liste-successor? [liste]
;;   "return true if liste = (range n)"
;;   (let [ new-liste (map vector liste (rest liste))]
;;     (reduce (fn [acc,pair]
;;               (and acc (= (inc (first pair))  (second pair))))
;;             true
;;             new-liste)))

(defn straight? [hand]
  (let [ vector-ranks  (hand-ranks hand)
         parser-ranks  [identity  #(replace {14 1} %)]
       ]
    (reduce (fn [acc,parser]
              (or acc (successor?(parser vector-ranks)))) false parser-ranks)))

;; (straight? two-pairs-hand)             ;=> false
;; (straight? straight-hand)              ;=> true
;; (straight? low-ace-straight-hand)      ;=> true
;; (straight? ["2H" "2D" "3H" "4H" "5H"]) ;=> false
;; (straight? high-ace-straight-hand)     ;=> true



(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

;; (straight-flush? straight-hand)                ;=> false
;; (straight-flush? flush-hand)                   ;=> false
;; (straight-flush? straight-flush-hand)          ;=> true
;; (straight-flush? low-ace-straight-flush-hand)  ;=> true
;; (straight-flush? high-ace-straight-flush-hand) ;=> true


(def liste-parser #{ [pair?  1]
                     [two-pairs? 2]
                     [three-of-a-kind? 3]
                     [straight? 4]
                     [flush?  5]
                     [full-house?  6]
                     [four-of-a-kind?  7]
                     [straight-flush?  8]})

(defn value [ hand ]
  (apply max (map (fn [pair]
                    (if ((first pair) hand)
                      (second pair)
                      0))
                  liste-parser)))

;; (value high-seven)           ;=> 0
;; (value pair-hand)            ;=> 1
;; (value two-pairs-hand)       ;=> 2
;; (value three-of-a-kind-hand) ;=> 3
;; (value straight-hand)        ;=> 4
;; (value flush-hand)           ;=> 5
;; (value full-house-hand)      ;=> 6
;; (value four-of-a-kind-hand)  ;=> 7
;; (value straight-flush-hand)  ;=> 8

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


