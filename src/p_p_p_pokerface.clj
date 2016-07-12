(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
    (Integer/valueOf (str fst))
    (get {\T 10, \J 11, \Q 12, \K 13, \A 14} fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (< 1 (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (< 2 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (< 3 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (= 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (let [freq (vals (frequencies (map rank hand)))]
    (or (= freq '(2 3))
        (= freq '(3 2)))))

(defn two-pairs? [hand]
  (let [freq (vals (frequencies (map rank hand)))]
    (or (= freq '(2 2 1))
        (= freq '(2 1 2))
        (= freq '(1 2 2)))))

(defn straight? [hand]
  (let [ma (apply max (map rank hand))
        mi (apply min (map rank hand))
        sortedhand (sort (map rank hand))]
        (if (and (== mi 2) (== ma 14))
          (= (sort (replace {14 1} sortedhand)) (range 1 6))
          (= sortedhand (range mi (+ ma 1))))))

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
  (apply max (map second (filter (fn [x] ((first x) hand)) checkers)))))
