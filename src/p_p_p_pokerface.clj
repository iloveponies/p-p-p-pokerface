(ns p-p-p-pokerface)

(defn rank [[r _ :as card]]
  (let [a {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (a r))))

(defn suit [[_ s :as card]]
  (str s))

(defn pair? [hand]
  (not (empty? (filter #(= 2 %) (vals (frequencies (map rank hand)))))))

(defn three-of-a-kind? [hand]
  (not (empty? (filter #(= 3 %) (vals (frequencies (map rank hand)))))))

(defn four-of-a-kind? [hand]
  (not (empty? (filter #(= 4 %) (vals (frequencies (map rank hand)))))))

(defn flush? [hand]
  (not (empty? (filter #(= 5 %) (vals (frequencies (map suit hand)))))))

(defn full-house? [hand]
  (and (three-of-a-kind? hand) (pair? hand)))

(defn two-pairs? [hand]
  (= 2 (count (filter #(= 2 %) (vals (frequencies (map rank hand)))))))

(defn straight? [hand]
  (let [a (sort (map rank hand))
        f (first a)
        b (map #(- % f) a)
        a2 (sort (replace {14 1} (map rank hand)))
        f2 (first a2)
        b2 (map #(- % f2) a2)]
    (or (= (range 0 5) b)
        (= (range 0 5) b2))))

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
    (apply max (map (fn [[f pt]] (if (f hand) pt 0)) checkers))))
