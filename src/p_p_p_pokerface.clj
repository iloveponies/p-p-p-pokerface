(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card
        replacements {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (Integer/valueOf (str (replacements fst))))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (let [freq-rank (vals (frequencies (map rank hand)))]
    (if (== (apply max freq-rank) 2)
      true
      false)))

(defn three-of-a-kind? [hand]
  (let [freq-rank (vals (frequencies (map rank hand)))]
    (if (== (apply max freq-rank) 3)
      true
      false)))

(defn four-of-a-kind? [hand]
  (let [freq-rank (vals (frequencies (map rank hand)))]
    (if (== (apply max freq-rank) 4)
      true
      false)))

(defn flush? [hand]
  (let [freq-suit (vals (frequencies (map suit hand)))]
    (if (== (apply max freq-suit) 5)
      true
      false)))

(defn full-house? [hand]
  (let [freq-rank (vals (frequencies (map rank hand)))]
    (if (and
         (== (apply max freq-rank) 3)
         (== (apply min freq-rank) 2))
      true
      false)))

(defn two-pairs? [hand]
  (let [freq-rank (vals (frequencies (map rank hand)))]
    (if (or
         (== (count freq-rank) 2)
         (and (== (count freq-rank) 3) (== (apply max freq-rank) 2)))
      true
      false)))

(defn straight? [hand]
  (let [map-hand      (map rank hand)
        sorted-hand   (sort map-hand)
        freq-rank     (vals (frequencies map-hand))
        max-freq-rank (apply max freq-rank)
        last-first    (- (last sorted-hand) (first sorted-hand))]
    (cond
     (and (== max-freq-rank 1) (== last-first 4))  true
     (and (== max-freq-rank 1) (== last-first 12)) true
     :else                                         false)))

(defn straight-flush? [hand]
  (if (and (flush? hand) (straight? hand))
    true
    false))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card?       0]
                   [pair?            1]
                   [two-pairs?       2]
                   [three-of-a-kind? 3]
                   [straight?        4]
                   [flush?           5]
                   [full-house?      6]
                   [four-of-a-kind?  7]
                   [straight-flush?  8]}
        hand-value (fn [vec] (if ((first vec) hand) (second vec) 0))]
    (apply max (map hand-value checkers))))
