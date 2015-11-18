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

(defn suit [[_ snd]]
  (str snd))

(defn rank [[fst _]]
  (if (Character/isDigit fst)
    (Integer/valueOf (str fst))
    (get {\T 10 \J 11 \Q 12 \K 13 \A 14} fst)))

(defn pair? [hand]
  (let [rank-freqs (frequencies (map rank hand))]
    (contains? (set (vals rank-freqs)) 2)))

(defn three-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (let [sorted-ranks (sort (vals (frequencies (map rank hand))))]
    (= sorted-ranks [2 3])))

(defn two-pairs? [hand]
  (let [sorted-ranks (sort (vals (frequencies (map rank hand))))]
    (or (= (rest sorted-ranks) [2 2])
        (= (rest sorted-ranks) [4]))))

(defn straight? [hand]
  (let [sorted-ranks    (sort (map rank hand))
        are-suits-same  (apply = (map suit hand))
        rank-seq        (if (= 2 (first sorted-ranks))
                          (sort (replace {14 1} sorted-ranks))
                          sorted-ranks)]
    (and (not are-suits-same)
         (= rank-seq
            (map (fn [x] (+ x (first rank-seq))) (range 0 5))))))

(defn straight-flush? [hand]
  (let [sorted-ranks    (sort (map rank hand))
        are-suits-same  (apply = (map suit hand))
        rank-seq        (if (= 2 (first sorted-ranks))
                          (sort (replace {14 1} sorted-ranks))
                          sorted-ranks)]
    (and are-suits-same
         (= rank-seq
            (map (fn [x] (+ x (first rank-seq))) (range 0 5))))))

(defn value [hand]
  (cond
    (straight-flush? hand)  8
    (four-of-a-kind? hand)  7
    (full-house? hand)      6
    (flush? hand)           5
    (straight? hand)        4
    (three-of-a-kind? hand) 3
    (two-pairs? hand)       2
    (pair? hand)            1
    :else                   0))
