(ns p-p-p-pokerface)

(defn hand [a-hand]
  a-hand)

(defn card [a-card]
  a-card)

(defn rank [card]
  (def ranks {\T 10, \J 11, \Q 12, \K 13, \A 14})
  (let [[r s] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (ranks r))))

(defn suit [card]
  (let [[r s] card]
    (str s)))

(defn same? [hand same-count]
  (= (apply max (vals (frequencies (map rank hand)))) same-count))

(defn pair? [hand]
  (same? hand 2))

(defn three-of-a-kind? [hand]
  (same? hand 3))

(defn four-of-a-kind? [hand]
  (same? hand 4))

(defn flush? [hand]
  (= (apply max (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (let [counts (vals (frequencies (map rank hand)))]
    (if (and (= (count counts) 2) (= (apply + counts) 5))
      (if (or (= (first counts) 2) (= (first counts) 3))
        true
        false)
      false)))

(defn two-pairs? [hand]
  (let [counts (vals (frequencies (map rank hand)))]
    (if (= (reduce + (filter even? counts)) 4)
      true
      false)))


(defn straight? [hand]
  (let [counts (sort (map rank hand))
        counts-repl (sort (replace {14 1} counts))]
    (if (= (first counts) 2)
      (= counts-repl (range (first counts-repl) (+ (first counts-repl) 5)))
      (= counts (range (first counts) (+ (first counts) 5))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1]
                  [two-pairs? 2] [three-of-a-kind? 3]
                  [straight? 4]  [flush? 5]
                  [full-house? 6] [four-of-a-kind? 7]
                  [straight-flush? 8]}]
    (apply max (map (fn [func]
      (let [f (first func)
            v (second func)]
        (if (f hand)
          v
          0))) checkers))))
