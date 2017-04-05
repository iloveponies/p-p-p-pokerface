(ns p-p-p-pokerface)

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn rank [card]
  (let [[rank _] card]
      (if (Character/isDigit rank)
            (Integer/valueOf (str rank))
            (get {\T 10, \J 11, \Q 12, \K 13, \A 14}, rank))))

(defn pair? [hand]
    (>= (apply max (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
    (>= (apply max (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
    (>= (apply max (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
    (= (apply max (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
    (let [freqs (sort (vals (frequencies (map rank hand))))]
    (= freqs (seq [2 3]))))

(defn two-pairs? [hand]
    (let [freqs (vals (frequencies (map rank hand)))]
    (or (= freqs (seq [1 4])) (= freqs (seq [2 2 1])))))

(defn straight? [hand]
  (let [hand-ranks (sort (map rank hand))]
  (or   (= hand-ranks (seq [2 3 4 5 14]))
        (= hand-ranks (range 10 15))
        (= hand-ranks (range 2 7))
        (= hand-ranks (range 3 8))
        (= hand-ranks (range 4 9))
        (= hand-ranks (range 5 10))
        (= hand-ranks (range 6 11))
        (= hand-ranks (range 7 12)
        (= hand-ranks (range 8 13))
        (= hand-ranks (range 9 14)))))) ; i'm sure there is some clever-ass way to do this, but if it works it's not stupid, right?

(defn straight-flush? [hand]
    (let [same-suit? (= 1 (count (set (map suit hand))))]
    (and same-suit? (straight? hand))))

(defn high-card? [hand]
    true) ; All hands have a high card.

(defn value [hand]
(let [checkers #{[high-card? 0] [pair? 1] [two-pairs? 2] [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
      hands (filter (fn[checker] (let [checker-function? (get checker 0)](checker-function? hand))) checkers) ; need to save fn reference into checker-function? instead of calling it explicitly (?)
      points-seq (map second hands)] ; pull out point values
                 (apply max points-seq))) ; return the max (not sure why this can't be done in a var)
