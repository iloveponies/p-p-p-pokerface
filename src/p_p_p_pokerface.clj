(ns p-p-p-pokerface)

(def my-high-seven                   ["2H" "3S" "4C" "5C" "7D"])
(def my-pair-hand                    ["2H" "2S" "4C" "5C" "7D"])
(def my-two-pairs-hand               ["2H" "2S" "4C" "4D" "7D"])
(def my-three-of-a-kind-hand         ["2H" "2S" "2C" "4D" "7D"])
(def my-four-of-a-kind-hand          ["2H" "2S" "2C" "2D" "7D"])
(def my-straight-hand                ["2H" "3S" "6C" "5D" "4D"])
(def my-low-ace-straight-hand        ["2H" "3S" "4C" "5D" "AD"])
(def my-high-ace-straight-hand       ["TH" "AS" "QC" "KD" "JD"])
(def my-flush-hand                   ["2H" "4H" "5H" "9H" "7H"])
(def my-full-house-hand              ["2H" "5D" "2D" "2C" "5S"])
(def my-straight-flush-hand          ["2H" "3H" "6H" "5H" "4H"])
(def my-low-ace-straight-flush-hand  ["2D" "3D" "4D" "5D" "AD"])
(def my-high-ace-straight-flush-hand ["TS" "AS" "QS" "KS" "JS"])

; a card is a string like "5C" or "JD"; 1st char is rank, 2nd char is suit

(defn rank [card]
  (let [[rank suit]  card
        replacements { \T 10, \J 11, \Q 12, \K 13, \A 14}]
    (cond
     (Character/isDigit rank) (Integer/valueOf (str rank))
     :else                    (get replacements rank))))


(defn suit [card]
  (let [[rank suit] card]
    (str suit)))

(defn pair? [hand]
  (> (apply max (vals (frequencies (map rank hand)))) 1))

(defn three-of-a-kind? [hand]
  (> (apply max (vals (frequencies (map rank hand)))) 2))

(defn four-of-a-kind? [hand]
  (> (apply max (vals (frequencies (map rank hand)))) 3))

(defn flush? [hand]
  (== (count (set (map suit hand))) 1))

(defn full-house? [hand]
  (=
   (sort (map #(get % 1) (frequencies (map rank hand))))
   '(2 3)))

(defn two-pairs? [hand]
  (let [f (sort (map #(get % 1) (frequencies (map rank hand))))]
    (cond
     (= f   '(1 2 2)) true
     (= f   '(2 3))   true
     (= f   '(1 4))   true
     :else            false)))

(defn straight? [hand]
  (let [ranks        (map rank hand)
        hi-ace-hand  (sort ranks)
        lo-ace-hand  (sort (replace {14 1} ranks))
        consecutive  (fn consecutive [lst]
                       (let [len (count lst)]
                         (cond
                          (= len 0) true
                          (= len 1) true
                          :else     (and (= 1 (apply - (reverse (take 2 lst))))
                                         (consecutive (rest lst))))))]
    (or (consecutive hi-ace-hand)
        (consecutive lo-ace-hand))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn value [hand]
  (cond
   (straight-flush?  hand)   8
   (four-of-a-kind?  hand)   7
   (full-house?      hand)   6
   (flush?           hand)   5
   (straight?        hand)   4
   (three-of-a-kind? hand)   3
   (two-pairs?       hand)   2
   (pair?            hand)   1
   :else                     0))
