(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card
        numerics {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (numerics r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn rank-cnt? [hand cnt]
  (= (apply max (vals (frequencies (map rank hand)))) cnt))

(defn pair? [hand]
  (or (rank-cnt? hand 2) (rank-cnt? hand 3) (rank-cnt? hand 4)))

(defn three-of-a-kind? [hand]
  (or (rank-cnt? hand 3) (rank-cnt? hand 4)))

(defn four-of-a-kind? [hand]
  (rank-cnt? hand 4))

(defn flush? [hand]
  (== (count (frequencies (map suit hand))) 1))

(defn full-house? [hand]
  (let [[c1 c2] (sort (vals (frequencies (map rank hand))))]
    (and (= c1 2) (= c2 3))))

(defn two-pairs? [hand]
  (or
    (four-of-a-kind? hand)
    (== 2 (count (filter (fn [x] (== 2 x)) (vals (frequencies (map rank hand))))))))

(defn straight? [hand]
  (let [sorted-hand (sort (map rank hand))
        get-min-rank (fn [x] (apply min x))
        replace-ace (fn [x] (sort (replace {14 1} x)))
        straight-hand (fn [x] (range x (+ x 5)))
        replaced-hand (replace-ace sorted-hand)]
    (cond
      (= sorted-hand (straight-hand (get-min-rank sorted-hand))) true
      (= replaced-hand (straight-hand (get-min-rank replaced-hand))) true
      :else false)))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        exec (fn [x] ((get x 0) hand))
        get-values (fn [x] (get x 1))]
    (apply max (map get-values (filter exec checkers)))))
