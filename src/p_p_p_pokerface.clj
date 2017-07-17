(ns p-p-p-pokerface)

(defn rankl [card]
  (let [[r _] card m {\T 10 \J 11 \Q 12 \K 13 \A 1}]
    (cond
      (Character/isDigit r) (Integer/valueOf (str r))
      :else (m r))))

(defn rank [card]
  (let [[r _] card m {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (cond
      (Character/isDigit r) (Integer/valueOf (str r))
      :else (m r))))

(defn suit [card]
  (let [[_ s] card] (str s)))

(defn freqs [c h] (frequencies (map c h)))

(defn pair? [hand]
  (contains?
    (set (vals (freqs rank hand)))
    2))

(defn three-of-a-kind? [hand]
  (contains?
    (set (vals (freqs rank hand)))
    3))

(defn four-of-a-kind? [hand]
  (contains?
    (set (vals (freqs rank hand)))
    4))

(defn flush? [hand]
  (= (first (vals (freqs suit hand))) 5))

(defn full-house? [hand]
  (= (sort (vals (freqs rank hand))) '(2 3)))

(defn two-pairs? [hand]
  (let [v (vals (freqs rank hand))
        vs (set v)
        twos (fn [x] (= x 2))]
    (or (contains? vs 4)
        (and
          (contains? vs 2)
          (= (count (filter twos v)) 2)))))

(defn straight? [hand]
  (let [rankedHi (sort (map rank hand))
        rankedLow (sort (map rankl hand))
        rangeHi (range (first rankedHi) (+ (count rankedHi) (first rankedHi)))
        rangeLow (range (first rankedLow) (+ (count rankedLow) (first rankedLow)))]
    (or (= rankedHi rangeHi) (= rankedLow rangeLow))))

(defn straight-flush? [hand] (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true)

(defn value [hand] (let [checkers [[high-card? 0] [pair? 1]
                                   [two-pairs? 2] [three-of-a-kind? 3]
                                   [straight? 4] [flush? 5]
                                   [full-house? 6] [four-of-a-kind? 7]
                                   [straight-flush? 8]]] (apply max (map (fn [r] (second r)) (filter (fn [r] (true? (first r))) (map (fn [c] [((first c) hand) (second c)]) checkers))))))

