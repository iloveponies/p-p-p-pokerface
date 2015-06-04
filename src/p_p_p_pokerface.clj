(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card
        above9 {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit r) (Integer/valueOf (str r)) (get above9 r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn rank-vals [hand]
  (vals (frequencies (map rank hand))))

(defn suit-vals [hand]
  (vals (frequencies (map suit hand))))

(defn max-of-a-kind [hand]
  (apply max (rank-vals hand)))

(defn of-a-kind? [hand n]
  (if(>= (max-of-a-kind hand) n) true false))

(defn pair? [hand]
  (of-a-kind? hand 2))

(defn three-of-a-kind? [hand]
  (of-a-kind? hand 3))

(defn four-of-a-kind? [hand]
  (of-a-kind? hand 4))

(defn flush? [hand]
  (if(>= (apply max (suit-vals hand)) 5) true false))

(defn full-house? [hand]
  (if(= (sort (rank-vals hand)) (seq [2 3])) true false))

(defn two-pairs? [hand]
  (let [rv (sort (rank-vals hand))]
    (if(or (= rv (seq [1 4])) (= rv (seq [1 2 2]))) true false)))

(defn straight? [hand]
  (let [hand-vals (sort (map rank hand))
        max-v (apply max hand-vals)
        min-v (apply min hand-vals)]
    (if(or
          (= hand-vals (range min-v (+ min-v 5)))
          (= hand-vals (seq [2 3 4 5 14]))
          (= hand-vals (seq [10 11 12 13 14])))
      true false)))

(defn straight-flush? [hand]
  (if (and (flush? hand) (straight? hand)) true false))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
      (apply max (map (fn [match] (second match)) (filter (fn [checker] ((first checker ) hand)) checkers)))))

