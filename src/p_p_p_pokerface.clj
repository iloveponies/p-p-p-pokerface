(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card
        values {\2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (values rank)))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (let [ranks (map rank hand)
        freq (frequencies ranks)
        highest ( apply max (vals freq))]
    (>= highest 2)))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freq (frequencies ranks)
        highest ( apply max (vals freq))]
    (>= highest 3)))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freq (frequencies ranks)
        highest ( apply max (vals freq))]
    (>= highest 4)))

(defn flush? [hand]
  (let [suit (map suit hand)
        freq (frequencies suit)
        highest (apply max (vals freq))]
    (>= highest 5)))

(defn full-house? [hand]
  (let [ranks (map rank hand)
        freq (sort (vals (frequencies ranks)))]
    (= freq [2 3])))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        freq (sort (vals (frequencies ranks)))
        highest (apply max freq)]
    (or (= freq [1 2 2])
        (>= highest 4))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        alt-ranks (replace {14 1} ranks)
        sorted (sort ranks)
        alt-sorted (sort alt-ranks)
        lowest (apply min sorted)
        alt-lowest (apply min alt-sorted)
        comp-list (range lowest (+ lowest 5))
        alt-comp-list (range alt-lowest (+ alt-lowest 5))]
    (or (= sorted comp-list)
        (= alt-sorted alt-comp-list))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn value [hand]
  (cond
    (straight-flush? hand)  8
    (four-of-a-kind? hand)  7
    (full-house? hand)      6
    (flush? hand)           5
    (straight? hand)        4
    (three-of-a-kind? hand) 3
    (two-pairs? hand)       2
    (pair? hand)            1
    :else                   0))

;---------;

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


;; (straight-flush? straight-hand)                ;=> false

