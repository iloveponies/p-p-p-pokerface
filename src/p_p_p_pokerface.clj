(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank-char _] card
        replacements {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (cond
      (Character/isDigit rank-char) (Integer/valueOf (str rank-char))
      :else (get replacements rank-char))))

;(rank "2H") ;=> 2
;(rank "4S") ;=> 4
;(rank "TS") ;=> 10
;(rank "JS") ;=> 11
;(rank "QS") ;=> 12
;(rank "KS") ;=> 13
;(rank "AS") ;=> 14

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

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

(defn rank-repeat-count [hand]
  (vals (frequencies (map rank hand))))

(defn suit-repeat-count [hand]
  (vals (frequencies (map suit hand))))

(defn has-n-repeats? [hand n]
  (< 0 (count (filter (fn [count] (= n count)) (rank-repeat-count hand)))))

(defn pair? [hand]
  (has-n-repeats? hand 2))

(defn three-of-a-kind? [hand]
  (has-n-repeats? hand 3))

(defn four-of-a-kind? [hand]
  (has-n-repeats? hand 4))

;(four-of-a-kind? two-pairs-hand)      ;=> false
;(four-of-a-kind? four-of-a-kind-hand) ;=> true

(defn flush? [hand]
  (= 5 (apply max (suit-repeat-count hand))))

; (flush? pair-hand)  ;=> false
; (flush? flush-hand) ;=> true)

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

; (full-house? three-of-a-kind-hand) ;=> false
; (full-house? full-house-hand)      ;=> true


(defn two-pairs? [hand]
  (or (four-of-a-kind? hand)
      (= '(2 2) (filter (fn [repeat] (= 2 repeat)) (rank-repeat-count hand)))))

; (two-pairs? two-pairs-hand)      ;=> true
; (two-pairs? pair-hand)           ;=> false
; (two-pairs? four-of-a-kind-hand) ;=> true

(defn sequential-ranks [rank-seq]
  (range (apply min rank-seq) (+ 1( apply max rank-seq))))

(defn straight? [hand]
  (let [r (sort (map rank hand))
        r-generated (sequential-ranks r)
        r-ace-low (sort (replace {14 1} r))
        r-ace-low-generated (sequential-ranks r-ace-low)]
    (or (= r r-generated)
        (= r-ace-low r-ace-low-generated))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (cond
    (straight-flush? hand) 8
    (four-of-a-kind? hand) 7
    (full-house? hand) 6
    (flush? hand) 5
    (straight? hand) 4
    (three-of-a-kind? hand) 3
    (two-pairs? hand) 2
    (pair? hand) 1
    :else 0))

; (value high-seven)           ;=> 0
; (value pair-hand)            ;=> 1
; (value two-pairs-hand)       ;=> 2
; (value three-of-a-kind-hand) ;=> 3
; (value straight-hand)        ;=> 4
; (value flush-hand)           ;=> 5
; (value full-house-hand)      ;=> 6
; (value four-of-a-kind-hand)  ;=> 7
; (value straight-flush-hand)  ;=> 8
