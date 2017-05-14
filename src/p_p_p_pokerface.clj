(ns p-p-p-pokerface)

;; (def high-seven ["2H" "3S" "4C" "5C" "7D"])
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

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (get {\T 10, \J 11, \Q 12, \K 13, \A 14} fst))))

(defn suit [card]
  ;; ignore first parameter of card
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (let [ranks (map rank hand)
        freq (vals (frequencies ranks))]
    (= 2 (apply max freq))))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freq (vals (frequencies ranks))]
    (= 3 (apply max freq))))

(defn four-of-a-kind? [hand] 
  (let [ranks (map rank hand)
        freq (vals (frequencies ranks))]
    (= 4 (apply max freq))))


(defn flush? [hand]
  (let [suits (map suit hand)
        freq (vals (frequencies suits))]
    (= 5 (apply max freq))))

(defn full-house? [hand]
  (let [ranks (map rank hand)
        freq (vals (frequencies ranks))]
    (= [2 3] (sort freq))
    ))

(defn two-pairs? [hand]
    (let [ranks (map rank hand)
        freq (vals (frequencies ranks))]
    (or (= [1 2 2] (sort freq))
        (= [1 4] (sort freq)))))

(defn straight? [hand]
  (let [ranks (sort (into [] (map rank hand)))
        ranks-with-low-ace (sort (into [] (replace {14 1} (into [] ranks))))]
    (cond
     (= 5 (apply max ranks-with-low-ace)) (=(into [] ranks-with-low-ace) (range 1 6))
     :else (let [lowest (apply min ranks)]
             (=(into [] ranks) (range lowest (+ lowest 5)))))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true)

(def checkers #{[high-card? 0]  [pair? 1]
                [two-pairs? 2]  [three-of-a-kind? 3]
                [straight? 4]   [flush? 5]
                [full-house? 6] [four-of-a-kind? 7]
                [straight-flush? 8]})

(defn value [hand]
  (let [check (fn [checker]
                (if ((first checker) hand)
                  (second checker) -1))]
    (apply max (map check checkers))))
