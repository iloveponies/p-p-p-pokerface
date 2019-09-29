(ns p-p-p-pokerface)

(defn rank [card]
  (def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14 })
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (replacements fst))))

(defn suit [card]
  (let [[_ snd] card] 
    (str snd)))

;; Hands!
; (def high-seven                   ["2H" "3S" "4C" "5C" "7D"])
; (def pair-hand                    ["2H" "2S" "4C" "5C" "7D"])
; (def two-pairs-hand               ["2H" "2S" "4C" "4D" "7D"])
; (def three-of-a-kind-hand         ["2H" "2S" "2C" "4D" "7D"])
; (def four-of-a-kind-hand          ["2H" "2S" "2C" "2D" "7D"])
; (def straight-hand                ["2H" "3S" "6C" "5D" "4D"])
; (def low-ace-straight-hand        ["2H" "3S" "4C" "5D" "AD"])
; (def high-ace-straight-hand       ["TH" "AS" "QC" "KD" "JD"])
; (def flush-hand                   ["2H" "4H" "5H" "9H" "7H"])
; (def full-house-hand              ["2H" "5D" "2D" "2C" "5S"])
; (def straight-flush-hand          ["2H" "3H" "6H" "5H" "4H"])
; (def low-ace-straight-flush-hand  ["2D" "3D" "4D" "5D" "AD"])
; (def high-ace-straight-flush-hand ["TS" "AS" "QS" "KS" "JS"])

(defn pair? 
  "Checks for two cards of the same rank"
  [hand]
  (not= 
    (count (map rank hand))
    (count (set (map rank hand)))))

(defn three-of-a-kind? 
  "Checks for three cards of the same rank"
  [hand]
  (< 2 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? 
  "Checks for four cards of the same rank"
  [hand]
  (< 3 (apply max (vals (frequencies (map rank hand))))))

(defn flush? 
  "Checks for five cards of the same suit"
  [hand]
  (> 3 (count (keys (frequencies (map suit hand))))))

(defn full-house? 
  "Checks for three cards of the same rank 
  and two cards of another rank"
  [hand]
  (= (sort (vals (frequencies (map rank hand)))) [2 3]))

(defn two-pairs? [hand]
  (or
    (= 
      (sort (vals (frequencies (map rank hand)))) [1 2 2]) 
      (four-of-a-kind? hand)))

(defn straight? 
  "Checks for five cards of sequential rank
  Ace can be both ranked 1 or 14. "
  [hand]
  (let [increasing? (fn [some-hand]
                    (= (range (apply min some-hand) 
                              (+ 1 (apply max some-hand)))
                       some-hand))
        high-ace-hand (sort (map rank hand))
        low-ace-hand (sort (replace {14 1} (map rank hand)))]
    (or
      (increasing? high-ace-hand)
      (increasing? low-ace-hand))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true)

(defn value 
  "Calculates the value of the whole hand"
  [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (let [ filtering (fn [matching] ((first matching) hand))]
      (apply max (map second (filter filtering checkers))))))
