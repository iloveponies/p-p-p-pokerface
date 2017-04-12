(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (get {\T 10 \J 11 \Q 12 \K 13 \A 14} rank))))

(defn suit [card]
  (let [ [_ suit] card]
    (str suit))) 


(defn pair? [hand]
  (= 2 (apply max (vals (frequencies (map rank hand))))))


(defn three-of-a-kind? [hand]
   (= 3 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
   (= 4 (apply max (vals (frequencies (map rank hand))))))


(defn flush? [hand]
  (->> (map suit hand)
      (frequencies )
      (vals ) 
      (apply max )
      (= 5 )))

(defn full-house? [hand]
  (->> (map rank hand)
       (frequencies)
       (vals)
       (sort)
       (= [2 3])))

(defn two-pairs? [hand]
  (->> (map rank hand)
       (frequencies)
       (vals)
       (sort)
       (= [1 2 2])))

(defn sorted-rank [hand]
  (sort (map rank hand)))

(defn straight? [hand]
  (let [sh (sort (map rank hand))]
    (if (= 14 (apply max (map rank hand)))
      (or (= sh [10 11 12 13 14] )
          (= sh [2 3 4 5 14] )
          )
      (=  sh (range (first sh) (+ 1 (last sh)))))
))

(defn contains-ace? [hand]
  (->> (map rank hand)
       (apply max)
       (= 14)))


(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

 (defn value [hand]
     (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
       (->> (map  (fn [x]
                    [((first x) hand) (second x)])  
                  checkers )
            (filter (fn [x]
                      (true? (first x))) )
            (map second )
            (apply max)
            )
  ))

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

