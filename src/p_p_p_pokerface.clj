(ns p-p-p-pokerface)

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

(defn rank [card]
  (let [[r _] card]
    (cond
      (Character/isDigit r)
        (Integer/valueOf (str r))
    :else
      (let [replacement {\T 10 \J 11 \Q 12 \K 13 \A 14}]
        (replacement r))
    )
  ))

(defn suit [card]
  (let [[_ s] card]
  (str s)))

(defn pair? [hand]
  (let [pure-freq (vals (frequencies (map rank hand)))
        filtered-freq (filter (fn [x] (= x 2)) pure-freq)]
    (not (empty? filtered-freq))
    ))

(defn three-of-a-kind? [hand]
  (let [pure-freq (vals (frequencies (map rank hand)))
    filtered-freq (filter (fn [x] (= x 3)) pure-freq)]
    (not (empty? filtered-freq))
    ))

(defn four-of-a-kind? [hand]
  (let [pure-freq (vals (frequencies (map rank hand)))
    filtered-freq (filter (fn [x] (= x 4)) pure-freq)]
    (not (empty? filtered-freq))
    ))

(defn flush? [hand]
  (let [suite-freq (vals (frequencies (map suit hand)))
        filtered-freq (filter (fn [x] (= x 5)) suite-freq)]
    (not (empty? filtered-freq))
    ))

(defn full-house? [hand]
  (and
    (pair? hand)
    (three-of-a-kind? hand)
    ))

(defn two-pairs? [hand]
  (let [pure-freq (vals (frequencies (map rank hand)))
        filtered-freq (filter (fn [x] (= x 2)) pure-freq)]
    (= (count filtered-freq) 2)
    ))

(defn straight? [hand]
  (let [AH-freq (map (fn [x] (if (= x 1) 14 x)) (map rank hand))
        AL-freq (map (fn [x] (if (= x 14) 1 x)) (map rank hand))
        maxAH   (apply max AH-freq)
        minAH   (apply min AH-freq)
        maxAL   (apply max AL-freq)
        minAL   (apply min AL-freq)
        diff-4  (fn [x y] (= 4 (- x y)))]
    (and
      (not (pair? hand))
      (not (two-pairs? hand))
      (not (three-of-a-kind? hand))
      (not (four-of-a-kind? hand))
      (or
        (diff-4 maxAH minAH)
        (diff-4 maxAL minAL))
    )))

(defn straight-flush? [hand]
  (and
    (straight? hand)
    (flush? hand)
    ))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        allowed-checkers (filter (fn [[x y]] (x hand)) checkers)
        allowed-values    (map second allowed-checkers)
        max-allowed-value (apply max allowed-values)]
    max-allowed-value
    ))
