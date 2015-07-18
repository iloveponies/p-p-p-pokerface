(ns p-p-p-pokerface)

(defn rank [card]
  (let [rank-letters {\T 10, \J 11, \Q 12, \K 13, \A 14}
        [r _] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (rank-letters r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn high-card? [hand]
  true)

(defn same-rank [hand]
  (apply max (vals (frequencies (map rank hand)))))

(defn pair? [hand]
  (>= (same-rank hand) 2))

(defn three-of-a-kind? [hand]
  (>= (same-rank hand) 3))

(defn four-of-a-kind? [hand]
  (>= (same-rank hand) 4))

(defn flush? [hand]
  (= (vals (frequencies (map suit hand))) '(5)))

(defn full-house? [hand]
  (= (sort (vals (frequencies (map rank hand)))) '(2 3)))

(defn two-pairs? [hand]
  (== (get (frequencies (vals (frequencies (map rank hand)))) 2 0) 2))

(defn straight? [hand]
  (let [ranks (map rank hand)
        low-ace (replace {14 1} ranks)
        check (fn [ranks] (let [sorted (sort ranks)]
                            (= sorted (range (first sorted) (+ (last sorted) 1)))))]
    (or (check ranks) (check low-ace))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (map second (filter #((first %) hand) checkers)))))
