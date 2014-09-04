(ns p-p-p-pokerface)

;; Makes a map of the non-numerical values, to access them easier.
(defn rank [card]
  (let [[val _] card
        non-numericals {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit val) 
      (Integer/valueOf (str val))
      (non-numericals val))))

(defn suit [card]
  (let [[_ s] card] 
   (str s)))


;;===============================================================
;;==========            TEST HANDS                 ==============
;;===============================================================
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
;;===============================================================
;;===============================================================
;;===============================================================

(defn same-kind? [hand n]
  (>= (apply max (vals (frequencies (map rank hand)))) n))

(defn pair? [hand]
  (same-kind? hand 2))

(defn three-of-a-kind? [hand]
  (same-kind? hand 3))

(defn four-of-a-kind? [hand]
  (same-kind? hand 4))

(defn flush? [hand]
  (== (apply max (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (= (sort (vals (frequencies (map rank hand)))) (range 2 4)))

(defn two-pairs? [hand]
  (or (= (sort (vals (frequencies (map rank hand)))) [1 2 2]) (four-of-a-kind? hand)))

(defn straight? [hand]
  (let [value-list-high (sort (map rank hand))
        value-list-low (sort (replace {14 1} (map rank hand)))
        min-value-high (apply min value-list-high)
        min-value-low (apply min value-list-low)
        ret-val (fn [value-list, min-value]
                  (= value-list (range min-value (+ min-value 5))))]
  (or (ret-val value-list-high min-value-high) (ret-val value-list-low min-value-low))))

(defn straight-flush? [hand]
  (if (flush? hand) (straight? hand) false))


(defn high-card? [hand]
  true) ; All hands have a high card.

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        apply-check (fn [checker] ((first checker) hand))
        filter-checks (filter apply-check checkers)
        get-values (map second filter-checks)]
    (apply max get-values)))
