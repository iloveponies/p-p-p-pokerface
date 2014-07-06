(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card
        rank-map {\T 10, \J 11, \Q 12, \K 13, \A 14 }]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (rank-map rank))))

(defn rank-low-ace [card]
  (let [[rank _] card
        rank-map {\T 10, \J 11, \Q 12, \K 13, \A 1 }]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (rank-map rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn of-a-suit? [quant hand]
  (let [suits (map suit hand)
        suit-freq (vals (frequencies suits))]
   (>= (apply max suit-freq) quant)))

(defn of-a-kind? [kind hand]
  (let [ranks (map rank hand)
        rank-freq (vals (frequencies ranks))]
   (>= (apply max rank-freq) kind)))

(defn pair? [hand]
  (of-a-kind? 2 hand))

(defn three-of-a-kind? [hand]
  (of-a-kind? 3 hand))

(defn four-of-a-kind? [hand]
  (of-a-kind? 4 hand))

(defn flush? [hand]
  (of-a-suit? 5 hand))

(defn ordered-ranks [hand]
 (let [ranks (map rank hand)
        rank-freq (vals (frequencies ranks))]
    (sort rank-freq)))

(defn full-house? [hand]
   (= (ordered-ranks hand) [2 3]))

(defn two-pairs? [hand]
  (or (= (ordered-ranks hand) [1 2 2])
      (= (ordered-ranks hand) [1 4])))

(defn straight? [hand]
  (let [ranks (map rank hand)
        ranks-low-ace (map rank-low-ace hand)
        min-rank (apply min ranks)
        min-low-ace-rank (apply min ranks-low-ace)]
        (or (= (sort ranks) (range min-rank (+ min-rank 5)))
            (= (sort ranks-low-ace) (range min-low-ace-rank (+ min-low-ace-rank 5))))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
(apply max (map second (filter #((first %) hand) checkers)))))
