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
  (let [[ranking _] card]
    (if (Character/isDigit ranking)
      (Integer/valueOf (str ranking))
      (get {\A 14 \K 13 \Q 12 \J 11 \T 10} ranking))))

(rank "QS")

(defn suit [card]
  (let [[_ suite] card]
    (str suite)))

(suit "2C")

(defn pair? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 2))

(pair? pair-hand)

(defn three-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 3))

(three-of-a-kind? three-of-a-kind-hand)

(defn four-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 4))

(four-of-a-kind? two-pairs-hand)

(defn flush? [hand]
  (apply = (map suit hand)))

(flush? flush-hand)

(defn full-house? [hand]
  (= (seq [2 3]) (sort (vals (frequencies (map rank hand))))))

(full-house? full-house-hand)

(defn two-pairs? [hand]
  (let [ranks (set (map rank hand))]
    (or
      (and
        (== 3 (count ranks))
        (not (three-of-a-kind? hand)))
      (== 2 (count ranks)))))

(two-pairs? two-pairs-hand)

(defn straight? [hand]
  (let [ranks (map rank hand)
        sorted-ranks (sort ranks)
        replaced (sort (replace {1 14} sorted-ranks))]
    (or
      (= (range (apply min ranks) (+ (apply min ranks) 5)) sorted-ranks)
      (= (range 1 6) replaced))))

(straight? straight-hand)

(defn straight-flush? [hand]
  (and
    (flush? hand)
    (straight? hand)))

(straight-flush? straight-flush-hand)

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        evals (filter (fn [check] ((first check) hand)) checkers)]
    (apply max (map second evals))))

(value straight-flush-hand)
