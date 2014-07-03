(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card
        replacement {\T 10,
                     \J 11,
                     \Q 12,
                     \K 13,
                     \A 14}]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (replacement r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn pair? [hand]
  (let [ranks (map rank hand)
        rank-freq (vals (frequencies ranks))]
    (> (apply max rank-freq) 1)))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
        rank-freq (vals (frequencies ranks))]
    (= (apply max rank-freq) 3)))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)
        rank-freq (vals (frequencies ranks))]
    (= (apply max rank-freq) 4)))

(defn flush? [hand]
  (let [suits (map suit hand)
        suit-freq (frequencies suits)]
    (= (count suit-freq) 1)))

(defn full-house? [hand]
  (let [ranks (map rank hand)
        rank-freq (sort (vals (frequencies ranks)))]
    (= rank-freq (range 2 4))))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        rank-freq (sort (vals (frequencies ranks)))]
    (or (four-of-a-kind? hand) (= rank-freq [1 2 2]))))

(defn straight? [hand]
  (let [rank-sorted (sort (map rank hand))
        fst (first rank-sorted)]
    (or (= [2 3 4 5 14] rank-sorted)
        (= (range fst (+ fst (count rank-sorted)))
           rank-sorted))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (map (fn [checker] (second checker))
                    (filter (fn [checker] ((first checker) hand))
                            checkers)))))
