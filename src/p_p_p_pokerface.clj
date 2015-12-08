(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card
        corr {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (get corr fst)
    )))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn n-of-a-kind? [n hand]
  (not (empty? (filter (fn [x] (> x (- n 1))) (vals (frequencies (map rank hand)))))))

(defn pair? [hand]
  (n-of-a-kind? 2 hand))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? 3 hand))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? 4 hand))

(defn flush? [hand]
  (not (empty? (filter (fn [x] (> x 4)) (vals (frequencies (map suit hand)))))))

(defn full-house? [hand]
  (= [2 3] (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (or
    (= [1 2 2] (sort (vals (frequencies (map rank hand)))))
    (four-of-a-kind? hand)))

(defn straight? [hand]
  (or (let [[a b c d e] (sort (map rank hand))]
    (and (= (- e 4) a) (< a b c d e)))
      (let [[a b c d e] (sort (replace {14 1} (map rank hand)))]
    (and (= (- e 4) a) (< a b c d e)))
      ))


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
  (apply max (map second (filter (fn [[x _]] (x hand)) checkers)))))
