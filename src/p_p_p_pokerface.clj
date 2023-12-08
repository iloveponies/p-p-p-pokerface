(ns p-p-p-pokerface)

(defn rank [card]
  (let [rank (let [[fst _] card] fst)]
  (cond (Character/isDigit rank)(Integer/valueOf (str rank))
        :else (get {\T 10, \J 11, \Q 12, \K 13, \A 14} rank))))

(defn suit [card]
  (str (let [[_ snd] card] snd)))

(defn card-rank [x]
  (let [[fst _] x] fst))

(defn high-card? [hand]
  true)

(defn pair? [hand]
  (= 2 (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (= 3 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (= 4 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (= 5 (apply max (vals(frequencies (map suit hand))))))

(defn full-house? [hand]
  (= [2 3] (sort(vals(frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (let [freq (sort(vals(frequencies (map rank hand))))]
    (or (= [1 2 2] freq ) (= [1 4] freq ))))

(defn straight? [hand]
  (let [sorted-a-f (sort (replace {14 1} (map rank hand))) sorted-a-l (sort (map rank hand))
        freq (= [1 1 1 1 1] (vals(frequencies (map rank hand))))]
  (or (and (= 4 (- (last sorted-a-f) (first sorted-a-f))) freq)
      (and (= 4 (- (last sorted-a-l) (first sorted-a-l))) freq))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
         if-pair (fn [[pair _]]
                 (pair hand))]
    (apply max (map last (filter if-pair checkers)))))
