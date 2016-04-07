(ns p-p-p-pokerface)

(defn rank [card]
  (let [replacements {\T 10, \J 11, \Q 12, \K 13, \A 14}
        [r _]        card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get replacements r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn n-of-a-kind? [hand n]
  (>= (apply max (vals (frequencies (map rank hand)))) n))

(defn pair? [hand]
  (n-of-a-kind? hand 2))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? hand 3))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? hand 4))

(defn flush? [hand]
  (== (apply max (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (= (sort (vals (frequencies (map rank hand)))) [2 3]))

(defn two-pairs? [hand]
  (let [freqs (sort (vals (frequencies (map rank hand))))]
    (or (= freqs [1 2 2]) (= freqs [1 4]))))

(defn straight? [hand]
  (let [ranks-A-14 (sort (map rank hand))
        ranks-A-1  (sort (replace {14 1} ranks-A-14))
        first-A-14 (first ranks-A-14)
        first-A-1  (first ranks-A-1)]
    (or (= ranks-A-14 (range first-A-14 (+ first-A-14 5)))
        (= ranks-A-1  (range first-A-1  (+ first-A-1  5))))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]      [pair? 1]
                   [two-pairs? 2]      [three-of-a-kind? 3]
                   [straight? 4]       [flush? 5]
                   [full-house? 6]     [four-of-a-kind? 7]
                   [straight-flush? 8]}
        category?  (fn [checker] ((first checker) hand))]
    (apply max (map second (filter category? checkers)))))
