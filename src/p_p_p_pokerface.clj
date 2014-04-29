(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card
        ranks {\2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9 \T 10 \J 11 \Q 12 \K 13 \A 14}]
    (ranks r)))

(defn suit [card]
  (let [[_ s] card] (str s)))

(defn has-same-rank-pattern [hand pattern]
  (= (sort (vals (frequencies (map rank hand)))) pattern))

(defn pair? [hand]
  (has-same-rank-pattern hand [1 1 1 2]))

(defn three-of-a-kind? [hand]
  (has-same-rank-pattern hand [1 1 3]))

(defn four-of-a-kind? [hand]
  (has-same-rank-pattern hand [1 4]))

(defn flush? [hand]
  (= (first (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (has-same-rank-pattern hand [2 3]))

(defn two-pairs? [hand]
  (or (has-same-rank-pattern hand [1 2 2])
      (has-same-rank-pattern hand [1 4])))

(defn straight? [hand]
  (let [sorted-hand (sort (map rank hand))]
    (or (= (range (first sorted-hand) (+ (first sorted-hand) 5)) sorted-hand)
        (= [2 3 4 5 14] sorted-hand))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn value [hand]
  (cond
   (straight-flush? hand) 8
   (four-of-a-kind? hand) 7
   (full-house? hand) 6
   (flush? hand) 5
   (straight? hand) 4
   (three-of-a-kind? hand) 3
   (two-pairs? hand) 2
   (pair? hand) 1
   :else 0
   ))

; Kumpikohan on helpompi ymmärtää???

(defn value2 [hand]
    (let [
          high-card? (fn [x] true)
          checkers #{[high-card? 0] [pair? 1] [two-pairs? 2] [three-of-a-kind? 3]
                    [straight? 4] [flush? 5] [full-house? 6] [four-of-a-kind? 7]
                    [straight-flush? 8]
                    }
          check-hand (fn [f] ((first f) hand))
          hand-value (fn [f] (second f))
         ]
    (apply max (map hand-value (filter check-hand checkers)))))

