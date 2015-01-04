(ns p-p-p-pokerface)

(defn rank [card]
  (let [ char-rank (first card)
        string-rank (str char-rank)
        rank-mapping {"T" 10 "J" 11 "Q" 12 "K" 13 "A" 14}
        is-digit? (Character/isDigit char-rank)]
    (if is-digit?
      (Integer/valueOf string-rank)
      (rank-mapping string-rank))))

(defn suit [card]
  (str (second card)))

(defn- equal-cards-one [hand]
  "return the number of same-rank cards in a hand"
  (let [ranks (map rank hand)]
    (apply max (vals (frequencies ranks)))))

(defn pair? [hand]
  (= 2 (equal-cards-one hand)))

(defn three-of-a-kind? [hand]
  (= 3 (equal-cards-one hand)))

(defn four-of-a-kind? [hand]
  (= 4 (equal-cards-one hand)))

(defn flush? [hand]
  (let [suits (map suit hand)
        same-suits (apply max (vals (frequencies suits)))]
    (if (= 5 same-suits)
      true
      false)))


(defn full-house? [hand]
  (let [rank (map rank hand)
        equal-cards (sort (vals (frequencies rank)))]
    (if (= '(2 3) equal-cards)
      true
      false)))

(defn two-pairs? [hand]
  (if (= 4(apply + (filter #(= 0 (mod % 2)) (vals
                                              (frequencies
                                                (map rank hand))))))
    true
    false))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        ranks-low-ace (sort (replace {14 1} ranks))
        min-rank (apply min ranks)
        min-rank-low-ace (apply min ranks-low-ace)
        a-flush (range min-rank (+ min-rank 5))
        a-flush-low-ace (range min-rank-low-ace (+ min-rank-low-ace 5))]
    (or (= ranks a-flush) (= ranks-low-ace a-flush-low-ace))))

(defn straight-flush? [hand]
  (let [suits (map suit hand)
        same-suit? (= 5 (first (vals (frequencies suits))))]
    (and same-suit? (straight? hand))))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
    (apply max (map (fn x [[f points]] (if (f hand) points 0)) checkers))))