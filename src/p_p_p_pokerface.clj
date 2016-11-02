(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (cond
        (= \T fst) 10
        (= \J fst) 11
        (= \Q fst) 12
        (= \K fst) 13
        (= \A fst) 14))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (if (contains? (set (vals (frequencies (map rank hand)))) 2)
    true
    false))

(defn three-of-a-kind? [hand]
  (if (contains? (set (vals (frequencies (map rank hand)))) 3)
    true
    false))

(defn four-of-a-kind? [hand]
  (if (contains? (set (vals (frequencies (map rank hand)))) 4)
    true
    false))

(defn flush? [hand]
  (if (contains? (set (vals (frequencies (map suit hand)))) 5)
    true
    false))

(defn full-house? [hand]
  (if (and (pair? hand) (three-of-a-kind? hand))
    true
    false))

(defn two-pairs? [hand]
 (if (or (= [1 2 2] (sort (vals (frequencies (map rank hand)))))
         (four-of-a-kind? hand))
   true
   false))

(defn straight? [hand]
  (let [rank-set (set (map rank hand))
        rank2-set (set (replace {14, 1} (map rank hand)))
        min-rank (apply min rank-set)
        max-rank (apply max rank-set)
        min2-rank (apply min rank2-set)
        max2-rank (apply max rank2-set)]
    (cond
      (pair? hand) false
      (three-of-a-kind? hand) false
      (four-of-a-kind? hand) false
      (= (- max-rank min-rank) 4) true
      (= (- max2-rank min2-rank) 4) true
      :else false)))

(defn straight-flush? [hand]
  (if (and (straight? hand) (flush? hand))
    true
    false))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]
                   [pair? 1]
                   [two-pairs? 2]
                   [three-of-a-kind? 3]
                   [straight? 4]
                   [flush? 5]
                   [full-house? 6]
                   [four-of-a-kind? 7]
                   [straight-flush? 8]}
        matches (filter (fn[x] ((first x) hand)) checkers)]
    (apply max (map second matches))))
