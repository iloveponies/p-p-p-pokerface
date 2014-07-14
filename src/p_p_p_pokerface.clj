(ns p-p-p-pokerface)

(def replacement {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[fst _] card
        rank-value (fn [rank] (if (Character/isDigit rank) (Integer/valueOf (str rank))))
        rank (rank-value fst)]
    (if (nil? rank) (replacement fst) rank)))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(comment
(def high-seven                   ["2H" "3S" "4C" "5C" "7D"])
(def pair-hand                    ["2H" "2S" "4C" "5C" "7D"])
(def two-pairs-hand               ["2H" "2S" "4C" "4D" "7D"])
(def three-of-a-kind-hand         ["2H" "2S" "2C" "4D" "7D"])
(def four-of-a-kind-hand          ["2H" "2S" "2C" "2D" "7D"])
(def straight-hand                ["2H" "3S" "6C" "5D" "4D"])
(def low-ace-straight-hand        ["2H" "3S" "4C" "5D" "AD"])
(def high-ace-straight-hand       ["TH" "AS" "QC" "KD" "JD"])
(def flush-hand                   ["2H" "4H" "5H" "9H" "7H"])
(def full-house-hand              ["2H" "5D" "2D" "2C" "5S"])
(def straight-flush-hand          ["2H" "3H" "6H" "5H" "4H"])
(def low-ace-straight-flush-hand  ["2D" "3D" "4D" "5D" "AD"])
(def high-ace-straight-flush-hand ["TS" "AS" "QS" "KS" "JS"])
  )

(defn n-kind [n hand]
  (if (some #(= n %) (vals (frequencies (map #(rank %) hand)))) true false))

(defn pair? [hand]
  (n-kind 2 hand))

(defn three-of-a-kind? [hand]
  (n-kind 3 hand))

(defn four-of-a-kind? [hand]
  (n-kind 4 hand))

(defn flush? [hand]
  (let [suits (map #(suit %) hand)]
    (apply = suits)))

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
