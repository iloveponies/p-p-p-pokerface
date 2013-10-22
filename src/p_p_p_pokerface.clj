(ns p-p-p-pokerface)


(def high-seven-1                   ["2H" "3S" "4C" "5C" "7D"])
(def pair-hand-1                    ["2H" "2S" "4C" "5C" "7D"])
(def two-pairs-hand-1               ["2H" "2S" "4C" "4D" "7D"])
(def three-of-a-kind-hand-1         ["2H" "2S" "2C" "4D" "7D"])
(def four-of-a-kind-hand-1          ["2H" "2S" "2C" "2D" "7D"])
(def straight-hand-1                ["2H" "3S" "6C" "5D" "4D"])
(def low-ace-straight-hand-1        ["2H" "3S" "4C" "5D" "AD"])
(def high-ace-straight-hand-1       ["TH" "AS" "QC" "KD" "JD"])
(def flush-hand-1                   ["2H" "4H" "5H" "9H" "7H"])
(def full-house-hand-1              ["2H" "5D" "2D" "2C" "5S"])
(def straight-flush-hand-1          ["2H" "3H" "6H" "5H" "4H"])
(def low-ace-straight-flush-hand-1  ["2D" "3D" "4D" "5D" "AD"])
(def high-ace-straight-flush-hand-1 ["TS" "AS" "QS" "KS" "JS"])

(def bad-hand ["2H" "2S" "4C" "5C" "7D"])
(def good-hand ["2H" "3H" "6H" "5H" "4H"])

(def ranks {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (if (Character/isDigit (first card))
    (Integer/valueOf (str (first card)))
    (ranks (first card))))

(defn suit [card]
  (str (second card)))

(defn n-pair [n hand]
  (not (empty? (filter (fn [x] (== x n))
                       (vals (frequencies (map rank hand)))))))

(defn high-card? [hand]
  true)

(defn pair? [hand]
  (n-pair 2 hand))

(defn three-of-a-kind? [hand]
  (n-pair 3 hand))

(defn four-of-a-kind? [hand]
  (n-pair 4 hand))

(defn flush? [hand]
  (== 1 (count (set (map suit hand)))))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [n-pairs (frequencies (vals (frequencies (map rank hand))))]
    (= 2 (n-pairs 2))))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))]
    (or (= (range (apply min ranks) (+ 5 (apply min ranks)))
           ranks)
        (= '(2 3 4 5 14)
           ranks))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn value [hand]
  (let [checkers [high-card?       pair?
                  two-pairs?       three-of-a-kind?
                  straight?        flush?
                  full-house?      four-of-a-kind?
                  straight-flush?]
        bool-values (map (fn [checker?] (checker? hand)) checkers)
        zipped (map vector bool-values (range 0 9))]
    (apply max (map second (filter first zipped)))))

