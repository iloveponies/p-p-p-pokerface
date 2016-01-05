(ns p-p-p-pokerface)

(defn rank [card]
  (def replacements {\T 10 \J 11 \Q 12 \K 13 \A 14})
  (let [[r _] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (replacements r)
      )))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn pair? [hand]
  (let [ranks (map rank hand)]
    (== 2 (apply max (vals (frequencies ranks))))))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (== 3 (apply max (vals (frequencies ranks))))))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (== 4 (apply max (vals (frequencies ranks))))))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (== 5 (apply max (vals (frequencies suits))))))

(defn full-house? [hand]
  (let [ranks (map rank hand)]
    (= (sort (vals (frequencies ranks))) [2 3])))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)]
    (or
     (four-of-a-kind? hand)
     (= (sort (vals (frequencies ranks))) [1 2 2]))))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        low-ace-ranks (replace {14 1} ranks)]
    (or
      (and
        (== 4 (- (apply max ranks) (apply min ranks)))
        (apply < ranks))
      (and
        (== 4 (- (apply max low-ace-ranks) (apply min low-ace-ranks)))
        (apply < ranks)))))


(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (defn checker-match-hand? [checker]
      (let [[predicate value] checker] (predicate hand)))
    (let [matched-checkers (filter checker-match-hand? checkers)]
      (apply max (map second matched-checkers)))))
