(ns p-p-p-pokerface
  )
(defn rank [card]
  (let [[rank _] card
        specials {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit rank)
      (Integer/parseInt (str rank))
      (get specials rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn n-cards [hand n & {:keys [by suit] :or {by rank}}]
  (->> hand
     (map by)
     frequencies
     vals
     sort
     reverse
     (take n)))

(defn pair? [hand]
     (= [2] (n-cards hand 1)))

(defn three-of-a-kind? [hand]
  (= [3] (n-cards hand 1)))

(defn four-of-a-kind? [hand]
  (= [4] (n-cards hand 1)))

(defn flush? [hand]
  (= [5] (n-cards hand 1 :by suit)))

(defn full-house? [hand]
  (= [3 2] (n-cards hand 2)))

(defn two-pairs? [hand]
  (= [2 2] (n-cards hand 2)))
(replace {14 1} [2 3 4 5 14])

(defn straight? [hand]
  (let [r (map rank hand)
        ranked (sort (if (= 5 (nth r 3)) (replace {14 1} r) r))
        s (n-cards hand 1 :by suit)]
  (and (>= (first s) 2)
       (= ranked (range (first ranked) (inc (last ranked)))))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
    (apply max (map (fn [[f p]] (if (f hand) p 0)) checkers))))

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
(def high-ace-straight-flush-hand ["TS" "AS" "QS" "KS" "JS"]))
