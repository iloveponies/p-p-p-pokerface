(ns p-p-p-pokerface)

(defn rank [card]
  (let [all-ranks {\T 10 \J 11 \Q 12 \K 13 \A 14}
        [rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (all-ranks rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (let [freqs (vals (frequencies (map rank hand)))]
    (or
     (== (apply max freqs) 2)
     (== (apply min freqs) 2))))

(defn three-of-a-kind? [hand]
  (let [freqs (vals (frequencies (map rank hand)))]
    (== (apply max freqs) 3)))

(defn four-of-a-kind? [hand]
  (let [freqs (vals (frequencies (map rank hand)))]
    (== (apply max freqs) 4)))

(defn flush? [hand]
  (let [freqs (vals (frequencies (map suit hand)))]
    (== (apply max freqs) 5)))

(defn full-house? [hand]
  (let [freqs (vals (frequencies (map rank hand)))]
    (= (sort freqs) (seq [2 3]))))

(defn two-pairs? [hand]
  (let [freqs (vals (frequencies (map rank hand)))]
    (= (sort freqs) (seq [1 2 2]))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        ranks-low-ace (replace {14 1} ranks)
        ideal-straight (range (apply min ranks)
                              (+ (apply max ranks) 1))
        ideal-straight-low-ace (range (apply min ranks-low-ace)
                                      (+ (apply max ranks-low-ace) 1))]
    (or
     (= (sort ranks) ideal-straight)
     (= (sort ranks-low-ace) ideal-straight-low-ace))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (map second
                    (filter (fn [[f _]] (f hand) ) checkers)))))
