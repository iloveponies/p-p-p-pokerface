(ns p-p-p-pokerface)

(defn rank [card]
  (let [given-rank (first card)]
    (let [rank-map {\T 10, \J 11, \Q 12, \K 13, \A 14}]
      (if (Character/isDigit given-rank)
        (Integer/valueOf (str given-rank))
        (get rank-map given-rank)))))

(defn suit [card]
  (str (second card)))

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
  (> (apply max (vals (frequencies (map rank hand)))) 1))

(defn three-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (contains? (set (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (and (contains? (set (vals (frequencies (map rank hand)))) 2)
       (contains? (set (vals (frequencies (map rank hand)))) 3))
  )

(defn two-pairs? [hand]
  (or (= [1 2 2] (sort (vals (frequencies (map rank hand)))))
      (= [1 4] (sort (vals (frequencies (map rank hand)))))))

(defn straight? [hand]
  (let [sorted (sort (map rank hand))
        sorted-with-replacement (if (and (= (apply min sorted) 2) (= 14 (apply max sorted)))
                                  (sort (replace {14 1} sorted))
                                  )]
    (if (not-empty sorted-with-replacement)
      (= (range (apply min sorted-with-replacement) (+ 1 (apply max sorted-with-replacement))) sorted-with-replacement)
      (= (range (apply min sorted) (+ 1 (apply max sorted))) sorted)
        )))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

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
    (high-card? hand) 0)
  ;(let [checkers #{[high-card? 0]  [pair? 1]
  ;                 [two-pairs? 2]  [three-of-a-kind? 3]
  ;                 [straight? 4]   [flush? 5]
  ;                 [full-house? 6] [four-of-a-kind? 7]
  ;                 [straight-flush? 8]}
  ;      given-hand (fn [checker] ((first checker) hand))]
  ; (apply max (map second (filter given-hand checkers))))
  )
