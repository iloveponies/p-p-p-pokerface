(ns p-p-p-pokerface)

(defn rank [card]
  (let [replacements {\T 10 \J 11 \Q 12 \K 13 \A 14}
        [rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (replacements rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (< 1 (apply max freqs))))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (< 2 (apply max freqs))))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (< 3 (apply max freqs))))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (apply = suits)))

(defn full-house? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (= [2 3] (sort freqs))))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        freqs (sort (vals (frequencies ranks)))]
    (or
      (= [1 2 2] freqs)
      (= [1 4] freqs))))

(defn straight? [hand]
  (let [ace-as-high (sort (map rank hand))
        ace-as-low (sort (replace {14 1} ace-as-high))]
    (or
      (= ace-as-high (range (apply min ace-as-high) (+ 1 (apply max ace-as-high))))
      (= ace-as-low (range (apply min ace-as-low) (+ 1 (apply max ace-as-low)))))))

(defn straight-flush? [hand]
  (and
    (straight? hand)
    (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        checked (map (fn [[checkFn value]]
                       (when (checkFn hand) value)) checkers)]
    (apply max (remove nil? checked))))

