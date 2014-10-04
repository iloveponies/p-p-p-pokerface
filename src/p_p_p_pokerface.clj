(ns p-p-p-pokerface)

(defn rank [card]
  (let [face-cards {\A 14
                    \K 13
                    \Q 12
                    \J 11
                    \T 10}
        [rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (Integer/valueOf (str (face-cards rank))))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (let [hand-ranks (map (fn [card] (rank card)) hand) ; get only card ranks
        same-rank-cards (vals (frequencies hand-ranks)) ; how many same rank cards
        pairs (map (fn [x] (if (== x 2) x)) same-rank-cards)] ; count pairs (not counting two pair)
        (if (= 1 (count (filter identity pairs)))
          true
          false)))

(defn three-of-a-kind? [hand]
  (let [hand-ranks (map (fn [card] (rank card)) hand)]
    (if (and
          (contains? (set (vals (frequencies hand-ranks))) 3)
          (not (pair? hand)))
      true
      false)))

(defn four-of-a-kind? [hand]
  (let [hand-ranks (map (fn [card] (rank card)) hand)]
    (contains? (set (vals (frequencies hand-ranks))) 4)))

(defn flush? [hand]
  (let [hand-suits (map (fn [card] (suit card)) hand)
          suit-occurrences (set (vals (frequencies hand-suits)))]
    (contains? (set (vals (frequencies hand-suits))) 5)))

(defn full-house? [hand]
  (let [hand-ranks (map (fn [card] (rank card)) hand)
        rank-occurrences (set (vals (frequencies hand-ranks)))]
  (if (and
        (contains? rank-occurrences 2)
        (contains? rank-occurrences 3))
    true
    false)))

(defn two-pairs? [hand]
  (let [hand-ranks (map (fn [card] (rank card)) hand) ; get only card ranks
        same-rank-cards (vals (frequencies hand-ranks)) ; how many same rank cards
        pairs (map (fn [x] (if (== x 2) x)) same-rank-cards)] ; count pairs
        (if (= 2 (count (filter identity pairs)))
          true
          false)))


(defn straight? [hand]
  (defn linear-line? [cards] ; for some reason did not work when defined in "let"
          (cond 
            (= (count cards) 1) true
            (= (first cards) (- (second cards) 1)) (linear-line? (rest cards))
            :else false))
  (let [hand-ranks (map (fn [card] 
                          (rank card)) hand)
        check-for-low-straight (fn [cards]
                                (if (and (contains? (set cards) 14)
                                          (contains? (set cards) 2))
                                  (conj (disj (set cards) 14) 1) ; remove 14 and add 1
                                  cards))]
    (if (linear-line? (sort (check-for-low-straight hand-ranks)))
      true
      false)))

(defn straight-flush? [hand]
  (if (and
        (straight? hand)
        (flush? hand))
    true
    false))

(defn high-card? [hand]
  true)

(defn value [hand]
  (cond
    (straight-flush? hand) 8
    (four-of-a-kind? hand) 7
    (full-house? hand) 6
    (flush? hand) 5
    (straight? hand) 4
    (three-of-a-kind? hand) 3
    (two-pairs? hand) 2
    (pair? hand) 1
    (high-card? hand) 0))
