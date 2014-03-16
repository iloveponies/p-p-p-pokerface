(ns p-p-p-pokerface)

(defn high-card? [hand]
  true)

(defn rank [card]
  (let [[rankchar _] card
        values {\T 10 \J 11 \Q 12 \K 13 \A 14}
        rank (if (Character/isDigit rankchar)
               (Integer/valueOf (str rankchar))
               (get values rankchar))]
    rank))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair?
  [hand]
  (= (count (frequencies (map rank hand)))
     4))

(defn three-of-a-kind? [hand]
  (= (apply max(vals (frequencies (map rank hand))))
     3))

(defn four-of-a-kind? [hand]
  (= (apply max(vals (frequencies (map rank hand))))
     4))

(defn flush? [hand]
  (= (count (frequencies (map suit hand)))
     1))

(defn full-house? [hand]
  (= (sort (vals (frequencies (map rank hand))))
     (seq [2 3])))

(defn two-pairs? [hand]
  (let [hand-values (vals (frequencies (map rank hand)))]
    (or
      (= (count (filter (fn [x] (= x 2)) hand-values))
         2)
      (= (count (filter (fn [x] (= x 4)) hand-values))
         1))))

(defn straight? [hand]
  (let [hand-values (sort (map rank hand))
        transformed-hand-values (if (= (apply min hand-values) 2)
                                  (sort (replace {14 1} hand-values))
                                  (sort hand-values))
        lowest-hand-value (apply min transformed-hand-values)
        straight-range (range lowest-hand-value (+ lowest-hand-value 5))]
    (= transformed-hand-values straight-range)))

(defn straight-flush? [hand]
  (and (flush? hand)
       (straight? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        eval-hand-fn (fn [x] (vector (second x) ((first x) hand)))
        eval-hand (map eval-hand-fn checkers)
        filtered-eval-hand (filter (fn [x] (not (= (second x) false))) eval-hand)
        hand-values (sort (map (fn [x] (first x)) filtered-eval-hand))]
    (apply max hand-values)))
