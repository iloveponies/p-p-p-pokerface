(ns p-p-p-pokerface)

(defn rank [card]
  (def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (replacements fst)
      )))

(defn suit [card]
  (str(let [[_ snd] card]
    snd)))

(defn amount-of-same-rank [hand]
  (apply max(vals (frequencies (map rank hand)))))

(defn pair? [hand]
  (==(amount-of-same-rank hand)2))

(defn three-of-a-kind? [hand]
  (==(amount-of-same-rank hand)3))

(defn four-of-a-kind? [hand]
  (==(amount-of-same-rank hand)4))

(defn flush? [hand]
  (==(apply max(vals (frequencies (map suit hand))))5))

(defn full-house? [hand]
  (= [2 3](sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (let [pairs (sort (vals (frequencies (map rank hand))))]
    (or (= [1 4]   pairs)
        (= [1 2 2] pairs))))

(defn straight? [hand]
  (let [cards (map rank hand)]
  (let [cards (sort (if (not= (first cards) 10)
                      (replace {14 1} cards)
                      cards))]
    (= (range (first cards) (+ (last cards) 1)) cards))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn hand-has-value? [hand value]
(let [checkers [high-card? pair? two-pairs? three-of-a-kind? straight?
                flush? full-house? four-of-a-kind? straight-flush?]]
  ((get checkers value) hand)))

(defn hand-has-type? [hand [checker value]]
  (and (checker hand) (hand-has-value? hand value)))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        hand-checkers (filter (fn [checker] (hand-has-type? hand checker)) checkers)]
        (apply max (map (fn [checker] (get checker 1)) hand-checkers))))
