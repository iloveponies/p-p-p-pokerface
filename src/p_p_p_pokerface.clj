(ns p-p-p-pokerface)

(def replacements {\T 10 \J 11 \Q 12 \K 13 \A 14})

(defn rank [[r _]]
  (if (Character/isDigit r)
    (Integer/valueOf(str r))
    (replacements r)))

(defn suit [[_ s]]
  (str s))

(defn max-same? [fun hand n]
  (= (apply max (vals (frequencies (map fun hand)))) n))

(defn max-same-rank? [hand n]
  (max-same? rank hand n))

(defn pair? [hand]
  (max-same-rank? hand 2))

(defn three-of-a-kind? [hand]
  (max-same-rank? hand 3))

(defn four-of-a-kind? [hand]
  (max-same-rank? hand 4))

(defn flush? [hand]
  (max-same? suit hand 5))

(defn sorted-rank-frequencies [hand]
  (sort (vals (frequencies (map rank hand)))))

(defn full-house? [hand]
  (let [ranks (sorted-rank-frequencies hand)]
    (= '(2 3) ranks)))

(defn two-pairs? [hand]
  (or (four-of-a-kind? hand)
      (= '(1 2 2) (sorted-rank-frequencies hand))))

(defn straight? [hand]
  (let [rank-sorted-hand (sort (map rank hand))
        first-rank (first rank-sorted-hand)
        compare-range (range first-rank (+ first-rank 5))]
    (or (= compare-range rank-sorted-hand)
        (let [ace-one-rank-sorted-hand (sort (replace {14 1} rank-sorted-hand))
              one-based-compare-range (range 1 6)]
          (= one-based-compare-range ace-one-rank-sorted-hand)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(def checkers #{;[high-card? 0]
                [pair? 1]
                [two-pairs? 2]
                [three-of-a-kind? 3]
                [straight? 4]
                [flush? 5]
                [full-house? 6]
                [four-of-a-kind? 7]
                [straight-flush? 8]})

(defn value [hand]
  (last (sort (map (fn [[check hand-value]]
                     (if (check hand)
                       hand-value
                       0))
                   checkers))))
