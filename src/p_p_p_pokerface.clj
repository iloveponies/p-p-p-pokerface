(ns p-p-p-pokerface)

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


(def rank->number {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank 
  [[r _]]
  (if (Character/isDigit r)
    (Integer/valueOf (str r))
    (get rank->number r)))

(defn suit 
  [[_ s]]
  (str s))

(defn group-cards
  "Group cards by either rank or suit. Hands are vectors of strings of the form
  form RankSuit as in ['2H' '3S' ...]"
  [hand rank-or-suit]
  (let [extracted (map rank-or-suit hand)]
    (frequencies extracted)))

(defn n-of-akind? 
  "Return true if the hand has n 'repeats' of the requested attribute: either 
  rank or suit."
  [n hand rank-or-suit]
  (let [test-for-n (fn [[_ cnt]] (= cnt n))]
    (not (empty? (filter test-for-n (group-cards hand rank-or-suit))))))

(defn pair? [hand]
  (n-of-akind? 2 hand rank))

(defn three-of-a-kind? [hand]
  (n-of-akind? 3 hand rank))

(defn four-of-a-kind? [hand]
  (n-of-akind? 4 hand rank))

(defn flush? [hand]
  (n-of-akind? 5 hand suit))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand) true))

(defn two-pairs? [hand]
  (let [test-for-n (fn [[_ cnt]] (= cnt 2))
        pair-groups (group-cards hand rank)]
    (or (four-of-a-kind? hand)
        (= 2 (count (filter test-for-n pair-groups))))))

(defn straight? [hand]
  (let [v1 (replace {14 1} (map rank hand))
        v2 (replace {1 14} (map rank hand))
        ordered1   (sort v1)
        ordered2   (sort v2)
        low-card1  (first ordered1)
        low-card2  (first ordered2)
        high-card1 (inc (last ordered1))
        high-card2 (inc (last ordered2))]
    (or (= (range low-card1 high-card1)
           ordered1)
        (= (range low-card2 high-card2)
           ordered2))))

(defn straight-flush? [hand]
  (and (flush? hand)
       (straight? hand)))

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
    :else 0))
