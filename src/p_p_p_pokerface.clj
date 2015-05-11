(ns p-p-p-pokerface)
;; TESTING POKER HANDS
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn rank [card]
  (let [[rank suit] card
        face-cards {\T 10
                    \J 11
                    \Q 12
                    \K 13
                    \A 14}]
    (cond (Character/isDigit rank) (Integer/valueOf (str rank))
          :else (get face-cards rank))))

(defn suit [card]
  (let [[rank suit] card]
    (str suit)))

(defn pair? [hand]
  (let [ranks (map rank hand)
        freq-vals (vals (frequencies ranks))]
    (if (some #(= 2 %) freq-vals)
      true
      false)))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freq-vals (vals (frequencies ranks))]
    (if (some #(= 3 %) freq-vals)
      true
      false)))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freq-vals (vals (frequencies ranks))]
    (if (some #(= 4 %) freq-vals)
      true
      false)))

(defn flush? [hand]
  (let [suits (map suit hand)
        distinct-suits (set suits)]
    (if (= (count distinct-suits) 1)
      true
      false)))

(defn full-house? [hand]
  (let [ranks (map rank hand)
        freq-vals (vals (frequencies ranks))
        distinct-ranks (set freq-vals)]
    (if (= (sort distinct-ranks) 
           (sort #{3 2}))
      true
      false)))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        freq-vals (vals (frequencies ranks))
        number-of-pairs (get (frequencies freq-vals) 2)]
    (if (or  (four-of-a-kind? hand)
               (= 2 number-of-pairs))
         true
         false)))

(defn straight? [hand]
  (let [ranks (map rank hand)
        low-ace (replace {14 1} ranks)
        sorted-ranks (sort ranks)
        sorted-low-ace (sort low-ace)]
    (if (or (= sorted-ranks
               (range (first sorted-ranks) (inc (last sorted-ranks))))
            (= sorted-low-ace
               (range (first sorted-low-ace) (inc (last sorted-low-ace)))))
      true
      false)))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

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
