(ns p-p-p-pokerface)

(def rank-values {\T 10 \J 11 \Q 12 \K 13 \A 14})

(defn hand [a-hand]
  a-hand)

(defn card [a-card]
  a-card)

(defn rank [a-card]
  (let [[r _] (card a-card)]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get rank-values r))))

(defn suit [a-card](str (second (card a-card))))
(defn ranks [a-hand] (map rank a-hand))
(defn suits [a-hand] (map suit a-hand))

(defn- n-of-a-kind? [a-hand n]
  (let [counts (->> a-hand ranks frequencies vals)]
    (boolean (some #(<= n %) counts))))

(defn pair? [a-hand] (n-of-a-kind? a-hand 2))
(defn three-of-a-kind? [a-hand] (n-of-a-kind? a-hand 3))
(defn four-of-a-kind? [a-hand] (n-of-a-kind? a-hand 4))

(defn flush? [a-hand]
  (let [counts (->> a-hand suits frequencies vals set)]
    (== 1 (count counts))))

(defn full-house? [a-hand]
  (let [counts-set (->> a-hand ranks frequencies vals set)]
    (and (contains? counts-set 3) (contains? counts-set 2))))

(defn two-pairs? [a-hand]
  (let [freqs-freqs (->> a-hand ranks frequencies vals frequencies)]
    (boolean (or
      (= 2 (get freqs-freqs 2))
      (and (get freqs-freqs 2) (get freqs-freqs 3))
      (get freqs-freqs 4)))))

(defn straight? [a-hand]
  (let [sorted-ranks (->> a-hand ranks sort vec)
        min-rank (first sorted-ranks)
        max-rank (last sorted-ranks)]
    (boolean (or
      (= sorted-ranks (range min-rank (+ 5 min-rank)))
      (and
        (== 14 max-rank)
        (= (subvec sorted-ranks 0 4) (range min-rank (+ 4 min-rank))))))))

(defn straight-flush? [a-hand]
  (and (straight? a-hand) (flush? a-hand)))

(defn value [a-hand]
  (cond
    (straight-flush? a-hand)  8
    (four-of-a-kind? a-hand)  7
    (full-house? a-hand)      6
    (flush? a-hand)           5
    (straight? a-hand)        4
    (three-of-a-kind? a-hand) 3
    (two-pairs? a-hand)       2
    (pair? a-hand)            1
    :else                     0))
