(ns p-p-p-pokerface)

(def replacements {\T 10,
                   \J 11,
                   \Q 12,
                   \K 13,
                   \A 14})

(defn rank [card]
  (let [[fst _] card
        is-digit (Character/isDigit fst)]
    (if is-digit
      (Integer/valueOf (str fst))
      (replacements fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn card-rank-freq [hand] (set (vals (frequencies (map rank hand)))))
(defn card-suit-freq [hand] (set (vals (frequencies (map suit hand)))))
(defn rank-freq [x hand] (set (vals (frequencies (filter (fn[y] (= y x)) (vals (frequencies (map rank hand))))))))

(defn pair? [hand]
    (contains? (card-rank-freq hand) 2))

(defn three-of-a-kind? [hand]
  (contains? (card-rank-freq hand) 3))

(defn four-of-a-kind? [hand]
  (contains? (card-rank-freq hand) 4))

(defn flush? [hand]
  (contains? (card-suit-freq hand) 5))

(defn full-house? [hand]
  (and (pair? hand)
       (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [contains-two-pair (contains? (rank-freq 2 hand) 2)
        contains-four-pair (contains? (rank-freq 4 hand) 1)]
    (or contains-two-pair
        contains-four-pair)))

(defn straight? [hand]
  (let [sorted-rank (fn[x] (sort (map rank x)))
        sorted-hand (sorted-rank hand)
        sorted-replaced-hand (sort (replace {14 1} sorted-hand))
        low (fn[x] (apply min (into [] x)))
        high (fn[x] (+ (apply max (into [] x)) 1))
        sorted-hand-range (range (low sorted-hand) (high sorted-hand))
        sorted-replaced-hand-range (range (low sorted-replaced-hand) (high sorted-replaced-hand))]
  (or (= sorted-hand sorted-hand-range)
      (= sorted-replaced-hand sorted-replaced-hand-range))))

(defn straight-flush? [hand]
  (and (straight? hand)
      (flush? hand)))

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
        filtered-checkers (filter (fn [x] ((first x) hand)) checkers)
        hand-values (map second filtered-checkers)]
    (apply max hand-values)))
