(ns p-p-p-pokerface)

(def arvot {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[arvo _] card]
    (if (Character/isDigit arvo)
      (Integer/valueOf (str arvo))
      (get arvot arvo))))



(defn suit [card]
  (let [[_ maa] card]
    (str maa)))

(suit "2H")
(rank "TD")


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


(defn pair? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 2))


;(apply max (vals (frequencies op(map rank high-seven))))


(defn three-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (= (apply max (vals (frequencies (map suit hand)))) 5))


(defn full-house? [hand]
  (= (sort (vals (frequencies (map rank hand)))) [2 3]))

;(full-house? pair-hand)

(defn two-pairs? [hand]
  (let [freqVec (sort (vals (frequencies (map rank hand))))]
    (or (= freqVec [1 2 2])
        (= freqVec [1 4]))))

;(two-pairs? two-pairs-hand)

(defn straight? [hand]
  (let [onko-suora? (fn [freqVec]
                      (= (range (apply min freqVec) (+ (apply max freqVec) 1))
                         freqVec)) ]
    (let [arvot (map rank hand)]
      (or (onko-suora? (sort arvot)) (onko-suora? (sort (replace {14 1} arvot)))))))

;(apply max (sort (vals (frequencies (map rank low-ace-straight-hand)))))

;(straight? pair-hand)

;(range (apply min (sort (map rank straight-hand))) (+ (apply max (sort (map rank straight-hand))) 1))


(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

;(straight-flush? high-ace-straight-flush-hand)

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
    (let [filter-funktio (fn [checkers-pari] ((first checkers-pari) hand))]
      (apply max (map second (filter filter-funktio checkers))))))

;(value high-ace-straight-hand)
