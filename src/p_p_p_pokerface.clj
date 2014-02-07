(ns p-p-p-pokerface)

(def replacement {\T 10, \J 11, \Q 12, \K 13, \A 14})

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

(defn rank [card]
  (let [[card-rank _] card
        rank->string (str card-rank)]
    (if (java.lang.Character/isDigit card-rank) 
      (java.lang.Integer/valueOf rank->string)
      (replacement card-rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (if (== (apply max (vals (frequencies (map rank hand)))) 2) true false))

(defn three-of-a-kind? [hand]
  (if (== (apply max (vals (frequencies (map rank hand)))) 3) true false))

(defn four-of-a-kind? [hand]
  (if (== (apply max (vals (frequencies (map rank hand)))) 4) true false))

(defn flush? [hand]
  (if (== (apply max (vals (frequencies (map suit hand)))) 5) true false))

(defn full-house? [hand]
  (if (= (seq (sort (vals (frequencies (map rank hand))))) (seq [2 3])) true false))

(defn two-pairs? [hand]
  (cond 
    (= (seq (sort (vals (frequencies (map rank hand))))) (seq [1 2 2])) true
    (four-of-a-kind? hand) false
    :else false))

(defn straight-helper? [test-hand] 
  (let [[a b c d e] test-hand]
    (if (= (+ a 4) (+ b 3) (+ c 2) (+ d 1) e) true false)))

(defn straight? [hand]
  (let [ace-replaced (fn [ranks] (replace {14 1} ranks))
        sorted-hand (fn [hand] (sort (map rank hand)))] 
    (if (== (count (sorted-hand hand))
            5) (or (straight-helper? (sorted-hand hand)) 
                   (straight-helper? (sort (ace-replaced (sorted-hand hand))))) false)))
  

(defn straight-flush? [hand]
  (if (and (straight? hand) (flush? hand)) true false))

(defn other-value [hand]
  (cond
    (and (pair? hand) (not (two-pairs? hand))) 1
    (two-pairs? hand) 2
    (and (three-of-a-kind? hand) (not (full-house? hand))) 3
    (and (straight? hand) (not (straight-flush? hand))) 4
    (and (flush? hand) (not (straight-flush? hand))) 5
    (full-house? hand) 6
    (four-of-a-kind? hand) 7
    (straight-flush? hand) 8
    :else 0))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        filter-func (fn [[function? val]] (if (function? hand) true false))]
    (apply max (map second (filter (fn [[function? val]] (if (function? hand) true false)) checkers)))))

