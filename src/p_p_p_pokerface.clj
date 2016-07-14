(ns p-p-p-pokerface)

(def replacements {\T 10 \J 11 \Q 12 \K 13 \A 14})

;; (def high-seven                   ["2H" "3S" "4C" "5C" "7D"])
;; (def pair-hand                    ["2H" "2S" "4C" "5C" "7D"])
;; (def two-pairs-hand               ["2H" "2S" "4C" "4D" "7D"])
;; (def three-of-a-kind-hand         ["2H" "2S" "2C" "4D" "7D"])
;; (def four-of-a-kind-hand          ["2H" "2S" "2C" "2D" "7D"])
;; (def straight-hand                ["2H" "3S" "6C" "5D" "4D"])
;; (def low-ace-straight-hand        ["2H" "3S" "4C" "5D" "AD"])
;; (def high-ace-straight-hand       ["TH" "AS" "QC" "KD" "JD"])
;; (def flush-hand                   ["2H" "4H" "5H" "9H" "7H"])
;; (def full-house-hand              ["2H" "5D" "2D" "2C" "5S"])
;; (def straight-flush-hand          ["2H" "3H" "6H" "5H" "4H"])
;; (def low-ace-straight-flush-hand  ["2D" "3D" "4D" "5D" "AD"])
;; (def high-ace-straight-flush-hand ["TS" "AS" "QS" "KS" "JS"])

(defn rank [[r _]]
  (if (Character/isDigit r)
    (Integer/valueOf (str r))
    (replacements r)))

(defn suit [[_ s]]
  (str s))

(defn n-of-a-kind? [n hand]
  (boolean (some (fn [v] (>= v n)) (vals (frequencies (map rank hand))))))

(defn pair? [hand]
  (n-of-a-kind? 2 hand))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? 3 hand))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? 4 hand))

(defn flush? [hand]
  (let [fst-suit (suit (first hand))]
    (every? (fn [h] (= fst-suit (suit h))) (rest hand))))

(defn full-house? [hand]
  (= (range 2 4) (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (or (four-of-a-kind? hand)
      (= '(1 2 2) (sort (vals (frequencies (map rank hand)))))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        min-rank (apply min ranks)
        max-rank (apply max ranks)]
    (if (and (== max-rank 14) (== min-rank 2))
      (= (range 1 6) (sort (replace {14 1} ranks)))
      (= (range min-rank (+ min-rank 5)) (sort ranks)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (cond (straight-flush? hand) 8
        (four-of-a-kind? hand) 7
        (full-house? hand) 6
        (flush? hand) 5
        (straight? hand) 4
        (three-of-a-kind? hand) 3
        (two-pairs? hand) 2
        (pair? hand) 1
        :else 0))
