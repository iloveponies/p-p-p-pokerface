(ns p-p-p-pokerface)

(defn rank [card]
  "card is a string with values for rank and suit."
  (let [[rank-char _] card
        rank-map {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit rank-char)
      (Integer/valueOf (str rank-char))
      (get rank-map rank-char))))

(defn suit [card]
  "card is a string with values for rank and suit."
  (let [[_ suit-char] card]
    (str suit-char)))

(defn pair? [hand]
  (let [hand-ranks (map rank hand)
        rank-counts (frequencies hand-ranks)]
    (if (> (apply max (vals rank-counts)) 1)
      true
      false)))

(defn three-of-a-kind? [hand]
  (let [rank-counts (frequencies (map rank hand))]
    (if (> (apply max (vals rank-counts)) 2)
      true
      false)))

(defn four-of-a-kind? [hand]
  (let [rank-counts (frequencies (map rank hand))]
    (if (> (apply max (vals rank-counts)) 3)
      true
      false)))

(defn flush? [hand]
  (let [hand-suits (frequencies (map suit hand))]
    (if (> (apply max (vals hand-suits)) 4)
      true
      false)))

(defn full-house? [hand]
  nil)

(defn two-pairs? [hand]
  nil)

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)

; TESTING DEFS!
;(def high-seven ["2H" "3S" "4C" "5C" "7D"])
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
