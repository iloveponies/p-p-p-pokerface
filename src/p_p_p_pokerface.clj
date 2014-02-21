(ns p-p-p-pokerface)

(def face-card->int {\T 10
                     \J 11
                     \Q 12
                     \K 13
                     \A 14})

(defn rank [card]
  (let [rank (first card)]
    (if (Character/isDigit rank)
      (Character/getNumericValue rank)
      (face-card->int rank))))

(defn suit [card]
  (str (second card)))

(defn rank-frequencies [hand]
  "Return the sorted count of sets within a hand"
  (let [card-ranks (map rank hand)
        rank-freqs (frequencies card-ranks)]
    (sort (vals rank-freqs))))

(defn max-set [hand]
  "Return the max size set of cards in hand"
  (apply max (rank-frequencies hand)))

(defn pair? [hand]
  (>= (max-set hand) 2))

(defn three-of-a-kind? [hand]
  (>= (max-set hand) 3))

(defn four-of-a-kind? [hand]
  (>= (max-set hand) 4))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (== (count (set suits)) 1)))

(defn full-house? [hand]
  (= (rank-frequencies hand) [2 3]))

(defn two-pairs? [hand]
  (let [rank-freqs (rank-frequencies hand)]
    (or (= rank-freqs [1 2 2])
        (= rank-freqs [1 4]))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        ace-low (replace {14 1} ranks)
        check (fn [ranks]
                (let [minv (apply min ranks)]
                  (= (sort ranks)
                     (range minv (+ minv 5)))))]
    (or (check ranks)
        (check ace-low))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn value [hand]
  (let [high-card? (fn [hand] true)
        checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        hand-values (map (fn [[check value]]
                           (if (check hand)
                             value
                             0))
                         checkers)]
    (apply max hand-values)))
