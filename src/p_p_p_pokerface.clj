(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card
        replacements {\T 10, \J 11, \Q 12, \K 13, \A 14}
        rank-string (str rank)
        rank-value (if (Character/isDigit rank) (Integer/valueOf rank-string) (replacements rank))]
    rank-value))

(defn sorted-ranks [hand]
  (sort (map rank hand)))

(defn sorted-ranks-ace-one [hand]
  (let [sranks (sorted-ranks hand)]
    (if (= (last sranks) 14) (cons 1 (butlast sranks)) sranks)))

(defn rank-frequencies [hand]
  (vals (frequencies (map rank hand))))

(defn sorted-rank-frequencies [hand]
  (sort (rank-frequencies hand)))

(defn unique-rank-frequencies [hand]
  (set (rank-frequencies hand)))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn suit-frequencies [hand]
  (vals (frequencies (map suit hand))))

(defn unique-suit-frequencies [hand]
  (set (suit-frequencies hand)))

(defn has-ace? [hand]
  (not (empty? (filter (fn [card] (= (rank card) 14) hand)))))

; Hand functions

(defn pair? [hand]
  (contains? (unique-rank-frequencies hand) 2))

(defn three-of-a-kind? [hand]
  (contains? (unique-rank-frequencies hand) 3))

(defn four-of-a-kind? [hand]
  (contains? (unique-rank-frequencies hand) 4))

(defn flush? [hand]
  (contains? (unique-suit-frequencies hand) 5))

(defn full-house? [hand]
  (= (sorted-rank-frequencies hand) [2 3]))

(defn two-pairs? [hand]
  (= (sorted-rank-frequencies hand) [1 2 2]))

(defn straight-low? [hand]
  (let [ranks (sorted-ranks-ace-one hand)
        min-rank (apply min ranks)
        required-range (range min-rank (+ min-rank 5))]
    (= required-range ranks)))

(defn straight-high? [hand]
  (let [ranks (sorted-ranks hand)
        min-rank (apply min ranks)
        required-range (range min-rank (+ min-rank 5))]
    (= required-range ranks)))

(defn straight? [hand]
  (or (straight-low? hand) (straight-high? hand)))

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
        check-hand (fn [checker] ((first checker) hand))]
    (apply max (map second (filter check-hand checkers)))))
