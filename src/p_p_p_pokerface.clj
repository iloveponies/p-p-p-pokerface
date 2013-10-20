(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank-character _] card
        ten-or-greater {\T 10, \J 11, \Q 12, \K 13, \A 14}]
  (if (Character/isDigit rank-character)
    (Integer/valueOf (str rank-character))
    (get ten-or-greater rank-character))))

(defn suit [card]
  (let [[_ suit-character] card]
    (str suit-character)))

(defn ranks [cards]
  (sort (map rank cards)))

(defn counts-of-each-rank [cards]
  (sort (vals (frequencies (ranks cards)))))

(defn n-of-a-kind? [n hand]
  (let [max-count-of-single-rank (apply max (counts-of-each-rank hand))]
    (== n max-count-of-single-rank)))

(defn pair? [hand]
  (n-of-a-kind? 2 hand))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? 3 hand))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? 4 hand))

(defn flush? [hand]
  (let [suits (fn [cards] (map suit cards))
        of-same-suit? (fn [cards] (apply = cards))]
    (of-same-suit? (suits hand))))

(defn full-house? [hand]
  (let [counts-in-full-house [2 3]]
    (= counts-in-full-house (counts-of-each-rank hand))))

(defn two-pairs? [hand]
  (let [counts-in-two-pairs [1 2 2]]
    (or (= counts-in-two-pairs (counts-of-each-rank hand))
        (four-of-a-kind? hand))))

(defn straight? [hand]
  (let [low-ace-hand (sort (replace {14 1} (ranks hand)))
        high-ace-hand (ranks hand)
        min-rank (fn [r] (apply min r))
        max-rank (fn [r] (apply max r))
        ranks-of-straight (fn [ranks-of-hand] (take 5 (range (min-rank ranks-of-hand) (+ 1 (max-rank ranks-of-hand)))))]
    (or (= low-ace-hand (ranks-of-straight low-ace-hand))
        (= high-ace-hand (ranks-of-straight high-ace-hand)))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn value [hand]
  nil)

