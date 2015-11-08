(ns p-p-p-pokerface)

(def replacement {\T 10, \J 11, \Q 12, \K 13, \A 14})
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
  (let [[fst] card]
  (if (Character/isDigit fst) (Integer/valueOf (str fst)) (replacement fst))))

(defn suit [card]
  (let [[_ snd] card] (str snd)))

(defn pair? [hand]
  (if (< 1 (apply max (vals (frequencies (map rank hand))))) true false))

(defn three-of-a-kind? [hand]
  (if (= 3 (apply max (vals (frequencies (map rank hand))))) true false))

(defn four-of-a-kind? [hand]
  (if (= 4 (apply max (vals (frequencies (map rank hand))))) true false))

(defn flush? [hand]
  (= 1 (count (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (if (= (range 2 4) (sort (vals (frequencies (map rank hand))))) true false))

(defn two-pairs? [hand]
  (if (or (= (seq [1 2 2]) (sort (vals (frequencies (map rank hand)))))
          (= (seq [1 4]) (sort (vals (frequencies (map rank hand)))))) true false))

(defn straight? [hand]
  (let [sorted (sort (map rank hand))
        min_val (first sorted)
        sorted_rpl (sort (replace {14 1} sorted))
        min_val_rpl (first sorted_rpl)]
    (or
     (= sorted (range min_val (+ min_val 5)))
     (= sorted_rpl (range min_val_rpl (+ min_val_rpl 5))))))

(defn straight-flush? [hand]
  (and
   (straight? hand)
   (flush? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        Check_and_get (fn [x]
                        (if ((first x) hand)
                          (second x) nil))
        values (map Check_and_get checkers)
        true_values (filter (fn [x] (not (nil? x))) values)]
    (apply max true_values)))
