(ns p-p-p-pokerface)

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

(defn rank [card]
  (let [[fst _] card]
    (cond
      (= "2" (str fst)) 2
      (= "3" (str fst)) 3
      (= "4" (str fst)) 4
      (= "5" (str fst)) 5
      (= "6" (str fst)) 6
      (= "7" (str fst)) 7
      (= "8" (str fst)) 8
      (= "9" (str fst)) 9
      (= "T" (str fst)) 10
      (= "J" (str fst)) 11
      (= "Q" (str fst)) 12
      (= "K" (str fst)) 13
      (= "A" (str fst)) 14
      :else 0)))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (let [freqVals (vals (frequencies (map rank hand)))]
    (= (apply max freqVals) 2)))

(defn three-of-a-kind? [hand]
  (let [freqVals (vals (frequencies (map rank hand)))]
    (= (apply max freqVals) 3)))

(defn four-of-a-kind? [hand]
  (let [freqVals (vals (frequencies (map rank hand)))]
    (= (apply max freqVals) 4)))

(defn flush? [hand]
  (let [freqVals (vals (frequencies (map suit hand)))]
    (= (apply max freqVals) 5)))

(defn full-house? [hand]
  (let [[fst sec] (vec (sort (vals (frequencies (map rank hand)))))]
    (and (= fst 2) (= sec 3))))

(defn two-pairs? [hand]
  (let [[fst sec] (vec (reverse (sort (vals (frequencies (map rank hand))))))]
    (or (and (= fst 2) (= sec 2)) (four-of-a-kind? hand))))


(defn straight? [hand]
  (let [hand-distinct-ranks (distinct (map rank hand))
        hand-distinct-ranks-ace-1 (replace {14 1} hand-distinct-ranks)
        min-ace-14 (apply min hand-distinct-ranks)
        max-ace-14 (apply max hand-distinct-ranks)
        min-ace-1 (apply min hand-distinct-ranks-ace-1)
        max-ace-1 (apply max hand-distinct-ranks-ace-1)
        ace-14-range (range (- max-ace-14 min-ace-14))
        ace-1-range (range (- max-ace-1 min-ace-1))
        ]
    (or (= (count ace-1-range) 4) (= (count ace-14-range) 4) )))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (cond 
   (and (not (two-pairs? hand))( pair? hand)) 1
    (and (not (four-of-a-kind? hand)) (two-pairs? hand)) 2
    (and (not (full-house? hand))  (three-of-a-kind? hand)) 3
    (and (straight? hand) (not (straight-flush? hand))) 4
    (and (flush? hand) (not (straight-flush? hand))) 5
    (full-house? hand) 6
    (four-of-a-kind? hand) 7
    (straight-flush? hand) 8
    :else 0))
