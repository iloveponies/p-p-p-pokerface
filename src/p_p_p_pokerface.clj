(ns p-p-p-pokerface)

(defn rank [card]
  (let [values {\T 10, \J 11, \Q 12, \K 13, \A 14}
        [fst _] card]
    (if (Character/isDigit fst) (Integer/valueOf (str fst)) (get values fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (let [ranks (map rank hand)
        group-sizes (vals (frequencies ranks))
        max-group-size (apply max group-sizes)]
    (not (== max-group-size 1))))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
        group-sizes (vals (frequencies ranks))
        max-group-size (apply max group-sizes)]
    (>= max-group-size 3)))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)
        group-sizes (vals (frequencies ranks))
        max-group-size (apply max group-sizes)]
    (>= max-group-size 4)))

(defn flush? [hand]
  (let [suits (map suit hand)
        suit-numbers (vals (frequencies suits))
        max-suit-number (apply max suit-numbers)]
    (== max-suit-number 5)))

(defn full-house? [hand]
  (let [ranks (map rank hand)
        rank-numbers (vals (frequencies ranks))
        max-rank-number (apply max rank-numbers)
        min-rank-number (apply min rank-numbers)]
    (and (== min-rank-number 2)
         (== max-rank-number 3))))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        rank-numbers (vals (frequencies ranks))
        sorted-rank-numbers (sort rank-numbers)]
    (not (== (second sorted-rank-numbers) 1))))
                  

(defn straight? [hand]
  (let [ranks (map rank hand)
        sorted-ranks (sort ranks)
        min-rank (first sorted-ranks)
        sorted-ranks-ace-as-one (replace {14 1} sorted-ranks)
        straight-from-min (range min-rank (+ min-rank 5))]
    (or (= sorted-ranks straight-from-min)
        (= sorted-ranks-ace-as-one (range 1 6)))))

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
