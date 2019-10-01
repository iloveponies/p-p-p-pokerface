(ns p-p-p-pokerface)

(def replacements {\T 10 \J 11 \Q 12 \K 13 \A 14})

(defn rank [card]
  (let [[rank _] card]
    (cond
      (Character/isDigit rank) (Integer/valueOf (str rank))
      :else (replacements rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn n-same? [hand function n]
  (>= (apply max (vals (frequencies (map function hand)))) n))

(defn pair? [hand]
  (n-same? hand rank 2))

(defn three-of-a-kind? [hand]
  (n-same? hand rank 3))

(defn four-of-a-kind? [hand]
  (n-same? hand rank 4))

(defn flush? [hand]
  (n-same? hand suit 5))

(defn full-house? [hand]
  (= [2 3] (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (= [1 2 2] (sort (vals (frequencies (map rank hand))))))

(defn straight? [hand]
  (let [incremental? (fn [ranks]
                       (= (range (apply min ranks) (+ (apply max ranks) 1)) (sort ranks)))]
    (or (incremental? (map rank hand)) (incremental? (replace {14 1} (map rank hand))))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1]
                   [two-pairs? 2] [three-of-a-kind? 3]
                   [straight? 4] [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (map second (filter (fn [[p _]] (p hand)) checkers)))))
