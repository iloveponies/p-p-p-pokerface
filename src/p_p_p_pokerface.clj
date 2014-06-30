(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank suit] card
        face->val {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (get face->val rank))))

(defn suit [card]
  (let [[rank suit] card]
    (str suit)))

(defn has-n? [hand n]
  (contains? (set (vals (frequencies (map rank hand)))) n))

(defn pair? [hand]
  (has-n? hand 2))

(defn three-of-a-kind? [hand]
  (has-n? hand 3))

(defn four-of-a-kind? [hand]
  (has-n? hand 4))

(defn flush? [hand]
  (contains? (set (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (and (pair? hand)
       (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (== 2 (count (filter #(== 2 %) (vals (frequencies (map rank hand)))))))

(defn straight? [hand]
  (let [rank-vals (sort (map rank hand))
        min-vals (apply min rank-vals)]
    (or (= rank-vals (range min-vals (+ min-vals 5)))
        (= (sort (replace {14 1} rank-vals)) (range 1 6)))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [hand-to-vals #{[high-card? 0] [pair? 1] [two-pairs? 2]
                       [three-of-a-kind? 3] [straight? 4] [flush? 5]
                       [full-house? 6] [four-of-a-kind? 7] [straight-flush? 8]}]
    (apply max (map #(if ((first %) hand) (second %) -1) hand-to-vals))))
