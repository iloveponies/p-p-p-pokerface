(ns p-p-p-pokerface)

(defn rank [card]
  (let [replacements {\T 10, \J 11, \Q 12, \K 13, \A 14}
        [rank _] card]
    (cond
      (Character/isDigit rank) (Integer/valueOf (str rank))
      :else (replacements rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (> (apply max (vals (frequencies (map rank hand)))) 1))

(defn three-of-a-kind? [hand]
  (> (apply max (vals (frequencies (map rank hand)))) 2))

(defn four-of-a-kind? [hand]
  (> (apply max (vals (frequencies (map rank hand)))) 3))

(defn flush? [hand]
  (= (apply max (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (let [ranks (map rank hand)]
    (= (sort (vals (frequencies ranks))) (seq [2 3]))))

(defn two-pairs? [hand]
  (let [freqs (vals (frequencies (map rank hand)))]
    (or
      (= 2 (nth freqs 0) (nth freqs 1))
      (= 4 (nth freqs 0)))))

(defn straight? [hand]
  (let [sorted-ranks (sort (map rank hand))
        minimum (apply min sorted-ranks)
        ace-sorted-ranks (sort (replace {14 1} sorted-ranks))]
    (or
      (= sorted-ranks (range minimum (+ minimum 5)))
      (= ace-sorted-ranks (range 1 6)))))

(defn straight-flush? [hand]
  (and
    (flush? hand)
    (straight? hand)))

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
