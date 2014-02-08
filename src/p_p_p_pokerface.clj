(ns p-p-p-pokerface)

(def replacements {\T 10 \J 11 \Q 12 \K 13 \A 14})

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

(defn rank [card]
  (let [[char _] card]
    (if (Character/isDigit char)
      (Integer/valueOf (str char))
      (replacements char))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn contains-N? [hand N]
  (->
   (vals (frequencies (map rank hand)))
   (set)
   (contains? N) ))

(defn pair? [hand]
  (contains-N? hand 2))

#_(defn pair? [hand]
  (if (= 2 (apply max (vals (frequencies (map rank hand)))))
    true
    false))

#_(defn pair? [hand]
  (let [ranks (map rank hand)
        rank-freq (vals (frequencies ranks))]
    (if (some #(= 2 %) rank-freq)
      true
      false)))

(defn three-of-a-kind? [hand]
  (contains-N? hand 3))

(defn four-of-a-kind? [hand]
  (contains-N? hand 4))

#_(defn flush? [hand]
  (let [suits (map suit hand)
        suit-count (first (vals (frequencies suits)))]
     (= suit-count 5)))

(defn flush? [hand]
  (->
   (map suit hand)
   (set)
   (count)
   (= 1)))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

#_(defn full-house? [hand]
 (= [2 3] (sort (vals (frequencies (map rank hand))))) )

(defn two-pairs? [hand]
  (let [rank-vals (vals (frequencies (map rank hand)))
        pairs-count (count (filter #(= 2 % ) rank-vals))]
    (= pairs-count 2)))

(defn straight? [hand]
  (let [rank-vals (map rank hand)
        f (fn [x] (let [y (sort x)]
                   (and (= 4 (- (last y) (first y)))
                        (= 5 (count (frequencies y))))))]
    (or (f rank-vals)
        (f (replace {14 1} rank-vals)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1]
                   [two-pairs? 2] [three-of-a-kind? 3]
                   [straight? 4] [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (->>
     (filter #((first %) hand) checkers)
     (map second)
     (apply max))))
