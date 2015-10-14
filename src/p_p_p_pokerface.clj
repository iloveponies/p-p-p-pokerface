(ns p-p-p-pokerface)

(declare rank)

(defn- ranks [hand]
  (map rank hand))

(defn- rank-frequencies [hand]
  (vals (frequencies (ranks hand))))

(def amax (partial apply max))

(defn- max-ranks-in-hand [hand]
  (amax (rank-frequencies hand)))

(defn- switch-ace [rs]
  (if (some #(= 2 %) rs)
    (replace {14 1} rs)
    rs))

(defn card [a-card]
  a-card)

(defn hand [a-hand]
  a-hand)

(defn rank [a-card]
  (let [ranks {\T 10, \J 11, \Q 12, \K 13, \A 14},
        [r _] (hand a-card)]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (ranks r))))

(defn suit [a-card]
  (let [[_ s] (card a-card)]
    (str s)))

(defn pair? [hand]
  (> (max-ranks-in-hand hand) 1))

(defn three-of-a-kind? [hand]
  (> (max-ranks-in-hand hand) 2))

(defn four-of-a-kind? [hand]
  (> (max-ranks-in-hand hand) 3))

(defn flush? [hand]
  (= 1 (count (set (map suit hand)))))

(defn full-house? [hand]
  (let [rf (rank-frequencies hand)]
    (and (= 2 (count rf)) (= [3 2] rf))))

(defn two-pairs? [hand]
  (let [f (rank-frequencies hand)]
    (or (= [2 2 1] f) (= [4 1] f) (= [5] f))))

(defn straight? [hand]
  (let [sorted-ranks (sort (switch-ace (ranks hand)))
        least-rank (first sorted-ranks)]
    (= sorted-ranks (range least-rank (+ 5 least-rank)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [val-map {high-card? 0, pair? 1, two-pairs? 2, three-of-a-kind? 3,
                 straight? 4, flush? 5, full-house? 6, four-of-a-kind? 7, straight-flush? 8},
        predicates (keys val-map)]
    (amax (map #(val-map %) (filter #(% hand) predicates)))))
