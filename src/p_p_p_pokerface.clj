(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get {\T 10, \J 11, \Q 12, \K 13, \A 14} r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn has-duplicate? [hand duplicate-size]
  (let [ranks (map rank hand)
        rank-frequencies (frequencies ranks)]
    (contains? (set (vals rank-frequencies)) duplicate-size)))

(defn pair? [hand]
  (has-duplicate? hand 2))

(defn three-of-a-kind? [hand]
  (has-duplicate? hand 3))

(defn four-of-a-kind? [hand]
  (has-duplicate? hand 4))

(defn flush? [hand]
  (let [suits (set (map suit hand))]
    (== 1 (count suits))))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        rank-frequencies (frequencies ranks)
        pair? (fn [kv] (== 2 (second kv)))]
    (== 2 (count (filter pair? rank-frequencies)))))

(defn straight? [hand]
  (let [ace-high (sort (map rank hand))
        ace-low (sort (replace {14 1} ace-high))
        straight-for (fn [sorted-ranks]
                       (let [minimum-rank (first sorted-ranks)]
                         (range minimum-rank (+ 5 minimum-rank))))]
    (or (= (straight-for ace-high) ace-high)
        (= (straight-for ace-low) ace-low))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn value [hand]
  (let [checkers #{[pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        apply-checker (fn [kv]
                        (if ((first kv) hand)
                          (second kv)
                          0))
        scores (map apply-checker checkers)]
    (apply max scores)))
