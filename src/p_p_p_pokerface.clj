(ns p-p-p-pokerface)

(def card-ranks {\2 2, \3 3, \4 4
            \5 5, \6 6, \7 7
            \8 8, \9 9, \T 10
            \J 11, \Q 12, \K 13
            \A 14})

(defn rank [card]
  (let [[fst _] card]
    (card-ranks fst)))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn hand-frequencies [hand]
  (vals (frequencies (map rank hand))))

(defn n-of-a-kind? [n hand]
  (boolean (some #(= n %) (hand-frequencies hand))))

(defn high-card? [hand]
  true)

(defn pair? [hand]
  (n-of-a-kind? 2 hand))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? 3 hand))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? 4 hand))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (or
   (four-of-a-kind? hand)
   (= [1 2 2] (sort (hand-frequencies hand)))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        is-straight? (fn [r]
                        (let [min-rank (apply min r)]
                          (= r (range min-rank (+ min-rank 5)))))]
    (or
     (is-straight? (sort ranks))
     (is-straight? (sort (replace {14 1} ranks))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        hand-value (fn [checker]
                     ((first checker) hand))]
    (apply max (map second (filter hand-value checkers)))))
