(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card
        upper-ranks {\T 10
                     \J 11
                     \Q 12
                     \K 13
                     \A 14}]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get upper-ranks r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn suits [hand]
  (map suit hand))

(defn ranks [hand]
  (map rank hand))

(defn freqs-ranks [hand]
  (vals (frequencies (ranks hand))))

(defn sorted-rank-freqs [hand]
  (sort (freqs-ranks hand)))

(defn biggest-most-frequent-rank [hand]
  (apply max (freqs-ranks hand)))

(defn pair? [hand]
  (== 2 (biggest-most-frequent-rank hand)))

(defn three-of-a-kind? [hand]
  (== 3 (biggest-most-frequent-rank hand)))

(defn four-of-a-kind? [hand]
  (== 4 (biggest-most-frequent-rank hand)))

(defn flush? [hand]
  (apply = (suits hand)))

(defn full-house? [hand]
  (= [2 3] (sorted-rank-freqs hand)))

(defn two-pairs? [hand]
  (or (= [1 2 2] (sorted-rank-freqs hand))
      (= [1 4] (sorted-rank-freqs hand))))

(defn straight? [hand]
  (let [r (ranks hand)]
    (or (= (sort r)
           (range (apply min r)
                  (inc (apply max r))))
        (= (sort (replace {14 1} r))
           (range (apply min (replace {14 1} r))
                  (inc (apply max (replace {14 1} r))))))))

(defn straight-flush? [hand]
  (and (flush? hand)
       (straight? hand)))

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
                   [straight-flush? 8]}
        check (fn [checker h]
                (if ((first checker) h)
                  (second checker)))
        check-all (fn [h]
                    (map #(check % h) checkers))
        sorted-vals (filter #(not (nil? %)) (check-all hand))]
    (apply max sorted-vals)))
