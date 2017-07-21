(ns p-p-p-pokerface)

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn rank [card]
  (let [[rank _] card
       replaces {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (cond
      (Character/isDigit rank) (Integer/valueOf (str rank))
      :else (replaces rank))))

(defn pair? [hand]
  (let [ran (map rank hand)
       fre    (vals (frequencies ran))]
    (>= (apply max fre) 2)))

(defn three-of-a-kind? [hand]
  (let [ran (map rank hand)
       fre    (vals (frequencies ran))]
    (>= (apply max fre) 3)))

(defn four-of-a-kind? [hand]
  (let [ran (map rank hand)
       fre   (vals (frequencies ran))]
    (>= (apply max fre) 4)))

(defn flush? [hand]
  (let [sui (map suit hand)
       fre    (vals (frequencies sui))]
    (== 1 (count fre))))

(defn full-house? [hand]
  (let [ran (map rank hand)
        fre (vals (frequencies ran))]
    (= (sort fre) [2 3])))

(defn two-pairs? [hand]
  (let [ran (map rank hand)
        fre (vals (frequencies ran))]
    (or (= (sort fre) [1 4]) (= (sort fre) [1 2 2]))))

(defn straight? [hand]
  (let [ran (map rank hand)
        sort1 (sort ran)
        sort2 (sort (replace {14 1} ran))
        min1 (apply min ran)
        min2 (apply min sort2)]
    (or   (= sort2 (range min2 (+ min2 5)))
          (= sort1 (range min1 (+ min1 5)))
          )))


(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))


(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        res (filter #((first %) hand) checkers)]
    (apply max (map second res))))

