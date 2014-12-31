(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card
        rank->int {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (rank->int rank))))


(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn has-n-of-a-kind? [hand n]
  (let [ranks (map rank hand)
        rank-freq-vals (vals (frequencies ranks))
        max-freq (apply max rank-freq-vals)]
    (= max-freq n)))

(defn pair? [hand]
  (has-n-of-a-kind? hand 2))

(defn three-of-a-kind? [hand]
  (has-n-of-a-kind? hand 3))

(defn four-of-a-kind? [hand]
  (has-n-of-a-kind? hand 4))

(defn flush? [hand]
  (let [suits (map suit hand)
        suit-freq-vals (vals (frequencies suits))
        max-freq (apply max suit-freq-vals)]
    (= max-freq 5)))

(defn matches-sorted-freqs [hand sorted-freqs]
  "checks whether the sorted frequencies of ranks of a
  hand matches the given sorted-freqs"
  (let [ranks (map rank hand)
        rank-freq-vals (vals (frequencies ranks))]
    (= sorted-freqs (sort rank-freq-vals))))

(defn full-house? [hand]
  (matches-sorted-freqs hand '(2 3)))

(defn two-pairs? [hand]
  (or (matches-sorted-freqs hand '(1 2 2))
      (matches-sorted-freqs hand '(1 4))))

(defn straight? [hand]
  "checks whether a hand is a straight aces high or low"
  (let [ranks (map rank hand)
        min-rank (apply min ranks)
        straight (range min-rank (+ min-rank 5))]
    (or (= (sort ranks) straight)
        (= (sort (replace {14 1} ranks)) (range 1 6)))))


(defn straight-flush? [hand]
  (and (flush? hand)
       (straight? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        matcher-for-checker (fn [checker] (first checker))
        value-for-checker (fn [checker] (second checker))
        hand-passed-checker? (fn [checker] ((matcher-for-checker checker) hand))
        passed-checkers (filter hand-passed-checker? checkers)
        values (map value-for-checker passed-checkers)]
    (apply max values)))

