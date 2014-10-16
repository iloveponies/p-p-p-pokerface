(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card
        replacements {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (replacements fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (let [score (vals (frequencies (map rank hand)))]
    (contains? (set score) 2)))

(defn three-of-a-kind? [hand]
  (let [score (vals (frequencies (map rank hand)))]
    (contains? (set score) 3)))

(defn four-of-a-kind? [hand]
  (let [score (vals (frequencies (map rank hand)))]
    (contains? (set score) 4)))

(defn flush? [hand]
  (let [score (keys (frequencies (map suit hand)))]
    (= 1 (count (set score)))))

(defn full-house? [hand]
  (let [score (vals (frequencies (map rank hand)))]
    (= #{3 2} (set score))))

(defn two-pairs? [hand]
  (let [score (sort (vals (frequencies (map rank hand))))]
    (= '(1 2 2) score)))

(defn straight? [hand]
  (let [ranks (map rank hand)
        aces-high (sort ranks)
        aces-low  (sort (replace {14 1} ranks))
        aces-high-start (first aces-high)
        aces-high-end   (+ aces-high-start 5)
        aces-high-range (range aces-high-start aces-high-end)
        aces-low-start (first aces-low)
        aces-low-end   (+ aces-low-start 5)
        aces-low-range (range aces-low-start aces-low-end)]
    (or (= aces-high-range aces-high)
        (= aces-low-range aces-low))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [_]
  true)

(defn value [hand]
  (let [scorers #{[high-card? 0]  [pair? 1]
                  [two-pairs? 2]  [three-of-a-kind? 3]
                  [straight? 4]   [flush? 5]
                  [full-house? 6] [four-of-a-kind? 7]
                  [straight-flush? 8]}
        passes (filter (fn [[f _]] (f hand)) scorers)
        scores (map second passes)
        high-score (apply max scores)]
    high-score))