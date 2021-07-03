(ns p-p-p-pokerface)

(def ranks {\T 10, \J 11, \Q 12, \K 13, \A 14})
;
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
;
;(def hands[high-seven pair-hand two-pairs-hand three-of-a-kind-hand four-of-a-kind-hand
;           straight-hand low-ace-straight-hand high-ace-straight-hand flush-hand
;           full-house-hand straight-flush-hand low-ace-straight-flush-hand
;           high-ace-straight-flush-hand])

(defn rank [card]
  (let[[value suit] card]
    (if (Character/isDigit value)
      (Integer/valueOf (str value))
      (get ranks value))))

(defn suit [card]
  (let[[value suit] card] (str suit)))

(defn high-card? [hand]
  true)

(defn pair? [hand]
  (not (nil? (some #(= % 2)
    (vals
      (frequencies
        (map rank hand)))))))

(defn three-of-a-kind? [hand]
  (not (nil? (some #(= % 3)
    (vals
      (frequencies
        (map rank hand)))))))

(defn four-of-a-kind? [hand]
  (not (nil? (some #(= % 4)
    (vals
      (frequencies
        (map rank hand)))))))

(defn flush? [hand]
  (not (nil? (some #(= % 5)
    (vals
      (frequencies
        (map suit hand)))))))

(defn full-house? [hand]
  (not (nil? (let [value-dist
    (vals
      (frequencies
        (map rank hand)))]
    (and
      (some #(= % 3) value-dist)
      (some #(= % 2) value-dist))))))

(defn two-pairs? [hand]
  (= 4 (apply +
         (filter (fn [value] (or (= 2 value) (= 4 value)))
           (vals
             (frequencies
               (map rank hand)))))))

(defn straight? [hand]
    (let
    [values (sort
      (mapv rank hand))]
    (let [smallest (apply min values)]
      (or
        (= values (range smallest (+ smallest 5)))
        (= (sort(replace {14 1} values)) (range 1 6))))))

(defn straight-flush? [hand]
  (and(straight? hand)(flush? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max
      (map
        (fn[cur-checker]
          (get cur-checker 1))
        (filter #((get % 0) hand)  checkers)))))