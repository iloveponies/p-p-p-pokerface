(ns p-p-p-pokerface)

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

(defn rank [card]
  (let [[r _] card
        values {\T "10" \J "11" \Q "12" \K "13" \A "14"}]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (Integer/valueOf (get values r)))))

(defn suit [card]
  (let [ [_ s] card]
    (str s)))

(defn freq-vals [hand type]
  (vals (frequencies (map type hand))))

(defn max-freq-vals [hand type]
  (apply max (freq-vals hand type)))

(defn pair? [hand]
  (>= (max-freq-vals hand rank) 2))

(defn three-of-a-kind? [hand]
  (== 3 (max-freq-vals hand rank)))

(defn four-of-a-kind? [hand]
  (== 4 (max-freq-vals hand rank)))

(defn flush? [hand]
  (== 5 (max-freq-vals hand suit)))

(defn full-house? [hand]
  (= (freq-vals hand rank) [3 2]))

(defn two-pairs? [hand]
  (or (= (freq-vals hand rank) [4 1])
  (= (freq-vals hand rank) [2 2 1])))

(defn sorted-straight? [ranks]
  (let [low (first ranks), high (+ low 5)]
    (= (range low high) ranks)))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        ranks-low (sort (replace {14 1} ranks))]
    (or (sorted-straight? ranks)
        (sorted-straight? ranks-low))))

(defn straight-flush? [hand]
  (and (flush? hand)
       (straight? hand)))

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
   :else 0))

