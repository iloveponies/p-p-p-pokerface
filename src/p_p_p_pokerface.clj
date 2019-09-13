(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rk _] card
        picval {\T 10
                \J 11
                \Q 12
                \K 13
                \A 14}]
    (if (Character/isDigit rk)
        (Integer/valueOf (str rk))
        (get picval rk))))

(defn suit [card]
  (let [[_ st] card]
    (str st)))

(defn pair? [hand]
  (let [hand-ranks (map rank hand)
        rank-freqs (vals (frequencies hand-ranks))]
    (>= (apply max rank-freqs) 2)))


(defn three-of-a-kind? [hand]
  (let [hand-ranks (map rank hand)
        rank-freqs (vals (frequencies hand-ranks))]
    (>= (apply max rank-freqs) 3)))

(defn four-of-a-kind? [hand]
   (let [hand-ranks (map rank hand)
        rank-freqs (vals (frequencies hand-ranks))]
    (>= (apply max rank-freqs) 4)))


(defn flush? [hand]
  (let [hand-suits (map suit hand)
        suit-freqs (vals (frequencies hand-suits))]
    (>= (apply max suit-freqs) 5)))

(defn full-house? [hand]
  (let [hand-ranks (map rank hand)
        rank-freqs (vals (frequencies hand-ranks))]
    (= (sort rank-freqs) (seq [2 3]))))


(defn two-pairs? [hand]
  (let [hand-ranks (map rank hand)
        rank-freqs (vals (frequencies hand-ranks))
        sorted-rank-freqs (sort rank-freqs)]
    (or
     (= sorted-rank-freqs (seq [1 2 2]))
     (= sorted-rank-freqs (seq [1 4])))))

(defn straight? [hand]
  (let [hand-ranks (map rank hand)
        sorted-hand-14 (sort hand-ranks)
        sorted-hand-1 (sort (replace {14 1} hand-ranks))]
    (or
     (= sorted-hand-1 (range (apply min sorted-hand-1) (+ (apply max sorted-hand-1) 1)))
     (= sorted-hand-14 (range (apply min sorted-hand-14) (+ (apply max sorted-hand-14) 1))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        helper (fn [pair] ((first pair) hand))
        possible-values (map second (filter helper checkers))]
    (apply max possible-values)))
