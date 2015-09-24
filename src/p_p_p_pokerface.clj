(ns p-p-p-pokerface)

(defn rank [card]
  (let [majors {\T 10 \J 11 \Q 12 \K 13 \A 14}
        rank (first card)]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (get majors rank))))

(defn suit [card]
  (str (last card)))

(defn ranks [hand]
  (mapv rank hand))

(defn rank-freqs [hand]
  (vals (frequencies (ranks hand))))

(defn max-rank-freq [hand]
  (apply max (rank-freqs hand)))

(defn pair? [hand]
  (> (max-rank-freq hand) 1))

(defn three-of-a-kind? [hand]
  (> (max-rank-freq hand) 2))

(defn four-of-a-kind? [hand]
  (> (max-rank-freq hand) 3))

(defn flush? [hand]
  (= (count (set (map suit hand))) 1))

(defn full-house? [hand]
  (= (vec (sort (rank-freqs hand))) [2 3]))

(defn two-pairs? [hand]
  (or (four-of-a-kind? hand)
      (= (vec (sort (rank-freqs hand))) [1 2 2])))

(defn straight? [hand]
  (letfn [(_straight? [hand-ranks]
    (let [next-is-bigger (fn [a b] (= (- b a) 1))
          first  (get hand-ranks 0)
          second (get hand-ranks 1)
          third  (get hand-ranks 2)
          fourth (get hand-ranks 3)
          fifth  (get hand-ranks 4)]
      (and (next-is-bigger first second)
           (next-is-bigger second third)
           (next-is-bigger third fourth)
           (next-is-bigger fourth fifth))))]
    (or (_straight? (vec (sort (ranks hand))))
        (_straight? (vec (sort (replace {14 1} (ranks hand))))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (defn high-card? [hand]
  true)
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
    (apply max (map (fn [checker] (second checker)) (filter (fn [checker] ((first checker) hand)) checkers)))))
