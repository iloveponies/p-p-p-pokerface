(ns p-p-p-pokerface)

(defn rank [card]
  (let [fst (first card)
        specials {\T 10, \J 11, \Q 12, \K 13, \A 14}]
  (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (get specials fst))))

(defn suit [card]
  (str (second card)))

(defn pair? [hand]
  (<= 1 (count (filter (fn [v] (= v 2)) (vals (frequencies (map rank hand)))))))

(defn three-of-a-kind? [hand]
  (= 1 (count (filter (fn [v] (= v 3)) (vals (frequencies (map rank hand)))))))

(defn four-of-a-kind? [hand]
  (= 1 (count (filter (fn [v] (= v 4)) (vals (frequencies (map rank hand)))))))

(defn flush? [hand]
  (= 1 (count (frequencies (map suit hand)))))

(defn full-house? [hand]
  (and (pair? hand)
       (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (= 2 (count (filter (fn [v] (= v 2)) (vals (frequencies (map rank hand)))))))

(defn straight? [hand]
  (let [values-ordered (sort (map rank hand))]
  (and (or (= 4 (- (last values-ordered) (first values-ordered)))
           (and (= 14 (last values-ordered)) (= 5 (nth values-ordered 3))))
       (apply < values-ordered))))

(defn straight-flush? [hand]
  (and (straight? hand)
        (flush? hand)))

(defn value [hand]
  (let [checkers #{[(fn [_] true) 0]
                   [pair? 1]
                   [two-pairs? 2]
                   [three-of-a-kind? 3]
                   [straight? 4]
                   [flush? 5]
                   [full-house? 6]
                   [four-of-a-kind? 7]
                   [straight-flush? 8]}]
  (last (sort (map last (filter (fn [[checker _]] (checker hand)) checkers))))))

