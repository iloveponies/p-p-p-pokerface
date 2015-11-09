(ns p-p-p-pokerface)

(def replacements {\T 10 \J 11 \Q 12 \K 13 \A 14})

(defn rank [card]
  (let [[r _] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get replacements r))))

(defn suit [card]
  (let [[_ s] card]
  (str s)))

(defn pair? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (= 1 (count (set (map suit hand)))))

(defn full-house? [hand]
  (= [2 3] (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (or
    (= 2 (count (filter (fn [x] (= x 2)) (vals (frequencies (map rank hand))))))
    (four-of-a-kind? hand)))

(defn straight? [hand]
  (let [ranked (map rank hand) min-val (apply min ranked) max-val (apply max ranked)
        replaced-rank (replace {14 1} ranked) min-replaced-val (apply min replaced-rank)
        max-replaced-val (apply max replaced-rank)]
       (and (= 5 (count (set ranked)))
            (or (= 4 (- max-val min-val)) (= 4 (- max-replaced-val min-replaced-val))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]} ]
  (apply max (map second (filter (fn [checker] ((first checker) hand)) checkers)))))
