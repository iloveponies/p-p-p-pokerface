(ns p-p-p-pokerface)

(defn rank [card]
  (let [[l-rank _]     card
        replacements   {\T 10
                        \J 11
                        \Q 12
                        \K 13
                        \A 14}
        l-rank-replaced (replacements l-rank)]
    (if (Character/isDigit l-rank)
      (Integer/valueOf (str l-rank))
      (Integer/valueOf (str l-rank-replaced)))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (let [pair              2
        ranks             (map rank hand)
        frequencies-ranks (vals (frequencies ranks))]
    (= pair (apply max frequencies-ranks))))

(defn three-of-a-kind? [hand]
  (let [three             3
        ranks             (map rank hand)
        frequencies-ranks (vals (frequencies ranks))]
    (= three (apply max frequencies-ranks))))

(defn four-of-a-kind? [hand]
  (let [four              4
        ranks             (map rank hand)
        frequencies-ranks (vals (frequencies ranks))]
    (= four (apply max frequencies-ranks))))

(defn flush? [hand]
  (let [suits          (set (map suit hand))
        ordered-ranks  (sort (map rank hand))
        min-rank       (first ordered-ranks)
        max-rank       (last ordered-ranks)]
    (= 1 (count suits))))

(defn full-house? [hand]
  (let [pair      2
        three     3
        ranks     (map rank hand)
        frequencies-ranks (set (vals (frequencies ranks)))]
    (boolean (and
     (get frequencies-ranks     pair)
     (get frequencies-ranks     three)))))

(defn two-pairs? [hand]
  (let [pair      2
        ranks     (map rank hand)
        frequencies-ranks (vals (frequencies ranks))]
    (if (four-of-a-kind? hand)
      true
      (= 2 (get (frequencies frequencies-ranks) pair)))))

(defn straight? [hand]
  (let [l-rank (sort (set (map rank hand)))
        l-rank-changed-ace  (sort (replace {14 1} l-rank))
        l-suit (set (map suit hand))]
     (and
      (or (= 4 (- (apply max l-rank) (apply min l-rank)))
          (= 4 (- (apply max l-rank-changed-ace) (apply min l-rank-changed-ace))))
      (= 5 (count l-rank)))))

(defn straight-flush? [hand]
  (and
   (flush? hand)
   (straight? hand)))

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
        executor (fn [x] (let [[function value] x]
                   (if (function hand)
                     value
                     nil)))]
    (apply max (filter number? (map executor checkers)))))

