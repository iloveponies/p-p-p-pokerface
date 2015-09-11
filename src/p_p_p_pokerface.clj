(ns p-p-p-pokerface)

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
  (let [[rnk _] card
        cardsBiggerThanNine {\T 10,\J 11,\Q 12,\K 13,\A 14}] 
    (if (Character/isDigit rnk) 
      (Integer/valueOf (str rnk))
      (Integer/valueOf (get cardsBiggerThanNine rnk)))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn x-of-a-kind? [x hand]
  (let [allRanks (map rank hand)]
    (= (apply max (vals (frequencies allRanks))) x)))

(defn pair? [hand]
  (x-of-a-kind? 2 hand))

(defn three-of-a-kind? [hand]
  (x-of-a-kind? 3 hand))

(defn four-of-a-kind? [hand]
  (x-of-a-kind? 4 hand))

(defn flush? [hand]
  (= (first (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (let [allRanks (map rank hand)]
    (and (= (count (frequencies allRanks)) 2) (not (four-of-a-kind? hand)))))

(defn two-pairs? [hand]
  (let [allRanks (map rank hand)]
    (or
      (= ((frequencies (vals (frequencies allRanks))) 2) 2) 
      (four-of-a-kind? hand))))

(defn straight? [hand]
  (let [currHand (sort (map rank hand))]
    (if (= currHand [10,11,12,13,14]) true
      (let [newHand (sort (replace {14 1} currHand))]
        (= newHand (range (first (min newHand)) (+ (apply max newHand) 1 )))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (cond
    (straight-flush? hand) 8
    (four-of-a-kind? hand) 7
    (full-house? hand) 6
    (two-pairs? hand) 2
    (pair? hand) 1
    (three-of-a-kind? hand) 3
    (straight? hand) 4
    (flush? hand) 5
    :else 0))
