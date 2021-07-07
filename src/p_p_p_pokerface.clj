(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r s] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (let [extras {\T 10 \J 11 \Q 12 \K 13 \A 14}]
        (extras r)))))

(defn suit [card]
  (let [[r s] card]
    (str s)))

(defn pair? [hand]
  (<= 2 (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (<= 3 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (<= 4 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (apply = (keys (frequencies (map suit hand)))))

(defn full-house? [hand]
  (= [2 3] (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (= [1 2 2] (sort (vals (frequencies (map rank hand))))))

(defn straight? [hand]
  (let [minimum (apply min (map rank hand))
        lo-flush (range 1 6)
        hi-flush (range minimum (+ 5 minimum))]
    (or (= lo-flush (sort (replace {14 1} (map rank hand))))
        (= hi-flush (sort (map rank hand))))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn value [hand]
  (let [high-card? (fn [hand] true)
        checkers #{[high-card? 0] [pair? 1]
                   [two-pairs? 2] [three-of-a-kind? 3]
                   [straight? 4] [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        points (filter #((first %) hand) checkers)
        valu (apply max (map second points))]
    valu))
