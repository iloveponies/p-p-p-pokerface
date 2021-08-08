(ns p-p-p-pokerface)

(defn rank [card]
  (let [face-card-values {\A 14 \K 13 \Q 12 \J 11 \T 10}
        card-rank-char (first card)
        card-rank-is-face-card? (Character/isDigit card-rank-char)]
    (if card-rank-is-face-card?
      (Integer/valueOf (str card-rank-char))
      (face-card-values card-rank-char ))))

(defn suit [card]
  (str (second card)))

(defn pair? [hand]
  (let [pair-amount 2
        hand-ranks (map rank hand)
        hand-rank-freq (-> hand-ranks frequencies vals)]
    (.contains hand-rank-freq pair-amount)))

(defn three-of-a-kind? [hand]
  (let [three-of-a-kind-amount 3
        hand-ranks (map rank hand)
        hand-rank-freq (-> hand-ranks frequencies vals)]
    (.contains hand-rank-freq three-of-a-kind-amount)))

(defn four-of-a-kind? [hand]
  (let [four-of-a-kind-amount 4
        hand-ranks (map rank hand)
        hand-rank-freq (-> hand-ranks frequencies vals)]
    (.contains hand-rank-freq four-of-a-kind-amount)))

(defn flush? [hand]
  (-> (map suit hand) frequencies vals (.contains 5)))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (or (> (count (filter #(= 2 %) (vals (frequencies (map rank hand))))) 1)
      (.contains (vals (frequencies (map rank hand))) 4)))

(defn straight? [hand]
  (let [hand-ranks-sorted (sort (map rank hand))
        start-rank (first hand-ranks-sorted)
        hand-ranks-sorted-low-ace (sort (replace {14 1} hand-ranks-sorted))]
    (or (= hand-ranks-sorted (range start-rank (+ start-rank 5)))
        (= hand-ranks-sorted-low-ace (range 1 6)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

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
      :else 0))
