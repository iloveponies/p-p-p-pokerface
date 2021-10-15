(ns p-p-p-pokerface)

(def ranks {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn high-card? [hand]
  true)

(defn rank [card]
  (let [[r _] card]
    (if (Character/isDigit r) (Integer/valueOf (str r))
    (ranks r))))

(defn suit [card]
  (let [[_ s] card]
  (str s)))

(defn pair? [hand]
  (< 1 (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (< 2 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (< 3 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (= 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
    (= [3 2] (seq (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (let [valis (vals (frequencies (map rank hand)))]
    (or
      (=
        [2 2 1]
        (seq valis)
      )
      (=
        [4 1]
        (seq valis)
    ))))

(defn straight? [hand]
  (let [sorted (keys (frequencies (sort (map rank hand))))
        sortedB (keys (frequencies (sort (replace {14 1} sorted))))]
    (or
      (and (= 5 (count sorted)) (= 4 (- (last sorted) (first sorted))))
      (and (= 5 (count sortedB)) (= 4 (- (last sortedB) (first sortedB))))
      )))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        check (fn [[checker reward]] (if (checker hand) reward nil))]
  (last (sort (map check checkers)))))
