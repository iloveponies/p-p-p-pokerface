(ns p-p-p-pokerface)

(def rankValues {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[fst _] card]
    (cond 
          (Character/isDigit fst) (Integer/valueOf (str fst))
          :else (rankValues fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

;;Debug data
;; (def high-seven                   ["0H" "3S" "4C" "5C" "7D"])
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

(defn get-max-rank-frequency-of-hand [hand]
  (apply max (vals (frequencies (map rank hand)))))

(defn pair? [hand]
  (<= 2 (get-max-rank-frequency-of-hand hand)))

(defn three-of-a-kind? [hand]
  (<= 3 (get-max-rank-frequency-of-hand hand)))

(defn four-of-a-kind? [hand]
  (<= 4 (get-max-rank-frequency-of-hand hand)))

(defn flush? [hand]
  (== 1 (count (frequencies (mapv suit hand)))))

(defn full-house? [hand]
  (and (= 3(apply max (vals (frequencies (mapv rank hand))))))
       (= 2(apply min (vals (frequencies (mapv rank hand))))))

(defn two-pairs? [hand]
  (= 2 (get (frequencies (vals (frequencies (map rank hand)))) 2 2)))


;;Messy TODO make nicer...
(defn straight? [hand] 
  (let [sortedList (sort (mapv rank hand))
        sortedReplacedList(sort (replace {14 1} sortedList))
        smallest (first sortedList)
        smallestReplacedList (first sortedReplacedList)
        reduceSmallest (fn [x] (- x smallest))
        reduceSmallestReplaced (fn [x] (- x smallestReplacedList))]
    (or (= (range 0 5) (mapv reduceSmallest sortedList))
        (= (range 0 5) (mapv reduceSmallestReplaced sortedReplacedList)))))

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
    (apply max (map second (filter (fn [x] ((first x) hand)) checkers)))))
