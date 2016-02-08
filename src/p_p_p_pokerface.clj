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

(defn suit [card]
 (str (let [[_ snd] card]
  snd)))

(defn rank [card]
     (let
      [[fst _] card]
      (if (Character/isDigit fst) (Integer/valueOf (str fst)) (Integer/valueOf (get {\A 14 \T 10 \J 11 \Q 12 \K 13} fst)))
    )
)

(defn pair? [hand]
  (contains?(set(vals (frequencies(map #(rank %) hand))))2))

(defn three-of-a-kind? [hand]
  (contains?(set(vals (frequencies(map #(rank %) hand))))3))

(defn four-of-a-kind? [hand]
  (contains?(set(vals (frequencies(map #(rank %) hand))))4))

(defn flush? [hand]
  (==(count(frequencies(map #(suit %) hand))) 1))

(defn full-house? [hand]
    (and (pair? hand)(three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (and (pair? hand)(==(count(frequencies(map #(rank %) hand))) 3)))

(defn straight? [hand]
  (let [kasi (sort (map #(rank %) hand))
        pienin (apply min kasi)
        uusikasi (if (and (== pienin 2))(sort(replace {14 1} kasi)) kasi)
        uusipienin (apply min uusikasi)]
    (= uusikasi (range uusipienin (+ uusipienin 5)))))

(defn straight-flush? [hand]
  (and (flush? hand)(straight? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
 (apply max(map #(second %)(filter #((first %) hand) checkers)))))


;(value high-seven)           ;=> 0
;(value pair-hand)            ;=> 1
;(value two-pairs-hand)       ;=> 2
;(value three-of-a-kind-hand) ;=> 3
;(value straight-hand)        ;=> 4
;(value flush-hand)           ;=> 5
;(value full-house-hand)      ;=> 6
;(value four-of-a-kind-hand)  ;=> 7
;(value straight-flush-hand)  ;=> 8
