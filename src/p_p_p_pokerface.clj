(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card
        rank-map {\T 10
                  \J 11
                  \Q 12
                  \K 13
                  \A 14}]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (rank-map r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn pair? [hand]
  (let [ranks     (map rank hand)
        hist      (vals (frequencies ranks))
        hist-hist (frequencies hist)]
    (= hist-hist
       {2 1
        1 3})))

(defn three-of-a-kind? [hand]
  (let [ranks     (map rank hand)
        hist      (vals (frequencies ranks))
        hist-hist (frequencies hist)]
    (= hist-hist
       {3 1
        1 2})))

(defn four-of-a-kind? [hand]
  (let [ranks     (map rank hand)
        hist      (vals (frequencies ranks))
        hist-hist (frequencies hist)]
    (= hist-hist
       {4 1
        1 1})))

(defn flush? [hand]
  (let [suits     (map suit hand)
        hist      (vals (frequencies suits))
        hist-hist (frequencies hist)]
    (= hist-hist
       {5 1})))

(defn full-house? [hand]
  (let [ranks     (map rank hand)
        hist      (vals (frequencies ranks))
        hist-hist (frequencies hist)]
    (= hist-hist
       {3 1
        2 1})))

(defn two-pairs? [hand]
  (let [ranks     (map rank hand)
        hist      (vals (frequencies ranks))
        hist-hist (frequencies hist)]
    (= hist-hist
       {2 2
        1 1})))

(defn straight? [hand]
  (let [hand-alt         (map (fn [card]
                                (replace {\A \1} card))
                              hand)
        ranks            (map rank hand)
        ranks-sorted     (sort ranks)
        ranks-alt        (map rank hand-alt)
        ranks-alt-sorted (sort ranks-alt)
        min-rank         (apply min ranks)
        max-rank         (apply max ranks)
        min-rank-alt     (apply min ranks-alt)
        max-rank-alt     (apply max ranks-alt)
        ranks-match      (range min-rank
                                (+ 1 max-rank))
        ranks-alt-match  (range min-rank-alt
                                (+ 1 max-rank-alt))]
    (and (or (= ranks-sorted
                ranks-match)
             (= ranks-alt-sorted
                ranks-alt-match)))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card?       0]
                   [pair?            1]
                   [two-pairs?       2]
                   [three-of-a-kind? 3]
                   [straight?        4]
                   [flush?           5]
                   [full-house?      6]
                   [four-of-a-kind?  7]
                   [straight-flush?  8]}]
    (apply max
           (map second
                (filter (fn [[checker-func point]]
                            (checker-func hand))
                        checkers)))))
