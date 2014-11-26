(ns p-p-p-pokerface)

(defn rank [card]
  (let [r (first card)]
    (case r
      \A 14
      \K 13
      \Q 12
      \J 11
      \T 10
      (Integer/valueOf (str r)))))

(defn suit [card]
  (str (second card)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn in?
  "true if seq contains elem"
  [seq elem]
  (= true (some #(= elem %) seq)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn card-frequencies [hand]
  (vals (frequencies (map rank hand))))

(defn pair? [hand]
  (in? (card-frequencies hand) 2))

(defn three-of-a-kind? [hand]
  (in? (card-frequencies hand) 3))

(defn four-of-a-kind? [hand]
  (in? (card-frequencies hand) 4))

(defn flush? [hand]
  (= 1 (count (set (map suit hand)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn full-house? [hand]
  (and
    (in? (card-frequencies hand) 3)
    (in? (card-frequencies hand) 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn two-pairs? [hand]
  (= 2 ((frequencies (card-frequencies hand)) 2)))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        low-ranks (sort (replace {14 1} ranks))]
    (or
      (let [lowest (apply min ranks)]
        (= ranks (range lowest (+ lowest 5))))
      (let [lowest (apply min low-ranks)]
        (= low-ranks (range lowest (+ lowest 5)))))))



(defn straight-flush? [hand]
  (and
    (straight? hand)
    (flush? hand)))

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
