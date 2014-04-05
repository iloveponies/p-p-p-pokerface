(ns p-p-p-pokerface)


(def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})



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
  (let [[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (replacements rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))


(defn amounts [hand]
 (set (vals (frequencies (map rank hand)))) )

(defn high-card? [hand]
  true)

(defn pair? [hand]
  (if (contains? (amounts hand) 2)
    true
    false))


(defn three-of-a-kind? [hand]
  (if (contains? (amounts hand) 3)
    true
    false))

(defn four-of-a-kind? [hand]
  (if (contains? (amounts hand) 4)
    true
    false))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (apply = suits)))



(defn full-house? [hand]
  (if (= (sort (amounts hand)) [2 3])
    true
    false))



(defn two-pairs? [hand]
  (let [freq (frequencies (vals (frequencies (map rank hand))))]
    (if (or (= 2 (get freq 2)) (= 1 (get freq 4)))
      true
      false)))


(defn straight? [hand]
  (let [smallest (apply min (map rank hand))
        next-five (range smallest (+ smallest 5))
        ranks (sort (map rank hand))
        ace-check [2 3 4 5 14]]
    (if (or (= ranks next-five)
            (= ranks ace-check))
      true
      false)))


(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))



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




;; (value high-seven)           ;=> 0
;; (value pair-hand)            ;=> 1
;; (value two-pairs-hand)       ;=> 2
;; (value three-of-a-kind-hand) ;=> 3
;; (value straight-hand)        ;=> 4
;; (value flush-hand)           ;=> 5
;; (value full-house-hand)      ;=> 6
;; (value four-of-a-kind-hand)  ;=> 7
;; (value straight-flush-hand)  ;=> 8
