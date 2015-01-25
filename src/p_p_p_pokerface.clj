(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card
        replacements {\T 10
                      \J 11
                      \Q 12
                      \K 13
                      \A 14}]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (replacements rank))))


(defn suit [card]
  (let [[_ suit] card]
    (str suit)))


(defn n-of-a-kind? [hand n]
  (contains? (set (vals (frequencies (map rank hand)))) n))

(defn pair? [hand]
  (n-of-a-kind? hand 2))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? hand 3))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? hand 4))

(defn flush? [hand]
  (contains? (set (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (== 2 (count (filter (fn [f] (= f 2)) (vals (frequencies (map rank hand)))))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        ace-one (sort (replace {14 1} ranks))
        ace-fourteen (sort ranks)]
    (or (= ace-one (range (first ace-one) (+ (apply max ace-one) 1)))
        (= ace-fourteen (range (first ace-fourteen) (+ (apply max ace-fourteen) 1))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]
                   [pair? 1]
                   [two-pairs? 2]
                   [three-of-a-kind? 3]
                   [straight? 4]
                   [flush? 5]
                   [full-house? 6]
                   [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (map second (filter (fn [[is-a? _]] (is-a? hand)) checkers)))))
