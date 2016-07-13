(ns p-p-p-pokerface)

(def ranks-ext {\T 10 \J 11 \Q 12 \K 13 \A 14})

(defn rank [card]
  (let [[r _] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get ranks-ext r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn cards-of-a-kind? [hand no-of-kind frequency]
  (let [rank-frequencies (vals (frequencies (map rank hand)))
          filter-fn (fn [x] (= x no-of-kind))]
    (if (= (count (filter filter-fn rank-frequencies)) frequency)
      true
      false)))

(defn pair? [hand]
  (cards-of-a-kind? hand 2 1))

(defn three-of-a-kind? [hand]
  (cards-of-a-kind? hand 3 1))

(defn four-of-a-kind? [hand]
  (cards-of-a-kind? hand 4 1))

(defn flush? [hand]
 (let [suit-frequencies (vals (frequencies (map suit hand)))
          filter-fn (fn [x] (= x 5))
        ]
    (if (= (count (filter filter-fn suit-frequencies) ) 1)
      true
      false)))

(defn full-house? [hand]
  (if (and (pair? hand) (three-of-a-kind? hand))
    true
    false))

(defn two-pairs? [hand]
  (or (cards-of-a-kind? hand 2 2) (four-of-a-kind? hand)))


(defn straight-hand? [hand operator-for-straight]
  (let [suit-frequency (vals (frequencies (map suit hand)))
        ranks-with-14 (sort (map rank hand))
        ranks-with-1 (sort (replace {14 1} ranks-with-14))
        min-ranks-with-14 (apply min ranks-with-14)
        max-ranks-with-14 (apply max ranks-with-14)
        min-ranks-with-1 (apply min ranks-with-1)
        max-ranks-with-1 (apply max ranks-with-1)
        rank-frequency (vals (frequencies ranks-with-14))]

    (if (and (operator-for-straight (count suit-frequency) 1) (= (count ranks-with-1) 5))
    (or (= ranks-with-14 (range min-ranks-with-14 (+ max-ranks-with-14 1)))
        (= ranks-with-1 (range min-ranks-with-1 (+ max-ranks-with-1 1))))
    false)))

(defn straight? [hand]
  (straight-hand? hand >))

(defn straight-flush? [hand]
  (and (straight-hand? hand =) (flush? hand)))

(defn high-card? [hand]
  (not (or
         (pair? hand)
         (three-of-a-kind? hand)
         (four-of-a-kind? hand)
         (flush? hand)
         (full-house? hand)
         (two-pairs? hand)
         (straight? hand)
         (straight-flush? hand))))


(defn value [hand]
    (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        filter-hand-value (fn [x] (let [[fnc? hand-val] x]
                                    (if (fnc? hand) x)))]

    (apply max (map (fn [x] (get x 1))(filter filter-hand-value checkers)))))
