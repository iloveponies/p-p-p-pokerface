(ns p-p-p-pokerface)

(def numeric-rank {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[rank suit] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (numeric-rank rank)
    )
  )
)

(defn minimum-rank [hand]
  (apply min (map rank hand))
)


(defn suit [card]
  (let [[rank suit] card]
    (str suit)  
  )
)
(defn pair? [hand]
  (= 2 (apply max (vals (frequencies (seq (map rank hand))))))
)

(defn three-of-a-kind? [hand]
  (= 3 (apply max (vals (frequencies (seq (map rank hand))))))
)

(defn four-of-a-kind? [hand]
  (= 4 (apply max (vals (frequencies (seq (map rank hand))))))
)

(defn flush? [hand]
  (= 5 (apply max (vals (frequencies (seq (map suit hand))))))
)

(defn full-house? [hand]
  (and 
    (= 3 (apply max (vals (frequencies (seq (map rank hand))))))
    (= 2 (apply min (vals (frequencies (seq (map rank hand))))))
  )
)

(defn two-pairs? [hand]
  (= [2 2 1] (vals (frequencies (seq (map rank hand)))))
)

(defn straight? [hand]
  (if (= 2 (minimum-rank hand))
    (or 
      (= (range 2 7) (sort (replace {14 1} (seq (map rank hand)))))
      (= (range 1 6) (sort (replace {14 1} (seq (map rank hand)))))
    )
    (= (range (minimum-rank hand) (+ (minimum-rank hand) 5)) (sort (seq (map rank hand))))
  )
)

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand))
)

(defn high-card? [hand]
  true)

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
    :else 0
  )
)


; Testing
; (def high-seven                   ["2H" "3S" "4C" "5C" "7D"])
; (def pair-hand                    ["2H" "2S" "4C" "5C" "7D"])
; (def two-pairs-hand               ["2H" "2S" "4C" "4D" "7D"])
; (def three-of-a-kind-hand         ["2H" "2S" "2C" "4D" "7D"])
; (def four-of-a-kind-hand          ["2H" "2S" "2C" "2D" "7D"])
; (def straight-hand                ["2H" "3S" "6C" "5D" "4D"])
; (def low-ace-straight-hand        ["2H" "3S" "4C" "5D" "AD"])
; (def high-ace-straight-hand       ["TH" "AS" "QC" "KD" "JD"])
; (def flush-hand                   ["2H" "4H" "5H" "9H" "7H"])
; (def full-house-hand              ["2H" "5D" "2D" "2C" "5S"])
; (def straight-flush-hand          ["2H" "3H" "6H" "5H" "4H"])
; (def low-ace-straight-flush-hand  ["2D" "3D" "4D" "5D" "AD"])
; (def high-ace-straight-flush-hand ["TS" "AS" "QS" "KS" "JS"])