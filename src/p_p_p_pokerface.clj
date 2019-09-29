(ns p-p-p-pokerface)

(defn hand [a-hand]
  a-hand)

(defn card [a-card]
  a-card)

(defn rank [card]
  (let [[r _] card]
    (cond
      (Character/isDigit r) (Integer/valueOf (str r))
      :else (get {\T 10, \J 11, \Q 12, \K 13, \A 14} r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn freq-rank [hand]
  "frequencies in hand by rank"
  (vals (frequencies (map rank hand))))

(defn max-freq-rank [hand]
  (apply max (freq-rank hand)))

(defn max-freq-suit [hand]
  (apply max (vals (frequencies (map suit hand)))))

(defn pair? [hand]
  (>=
    (max-freq-rank hand)
     2))

(defn three-of-a-kind? [hand]
  (>= (max-freq-rank hand) 3))

(defn four-of-a-kind? [hand]
  (>= (max-freq-rank hand) 4))

(defn flush? [hand]
  (= (max-freq-suit hand) 5))

(defn full-house? [hand]
  (= (sort (freq-rank hand)) [2 3]))

(defn two-pairs? [hand]
  (or
     (= (sort (freq-rank hand)) [1 2 2])
     (four-of-a-kind? hand)))

(defn straight? [hand]
  (let [ranks (map rank hand)
        ranks-a
        (if (= (apply min ranks) 2)
          (replace {14 1} ranks)
          ranks)]
  (and
   (= (freq-rank hand) [1 1 1 1 1])
   (= (- (apply max ranks-a)(apply min ranks-a)) 4))))

(defn straight-flush? [hand]
  (and
   (flush? hand)
   (straight? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [test-hand (fn [x] ((first x) hand))
        checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        hands (filter test-hand checkers)]
    (apply max (map second hands))))
