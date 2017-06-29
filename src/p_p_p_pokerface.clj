(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card]
    (cond
      (= \T rank) 10
      (= \J rank) 11
      (= \Q rank) 12
      (= \K rank) 13
      (= \A rank) 14
      :else (Integer/valueOf(str rank)))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn card-counts [hand]
  (vals (frequencies (map rank hand))))

(defn amount-same-rank [hand]
  (apply max (card-counts hand)))

(defn pair? [hand]
  (>= (amount-same-rank hand) 2))

(defn three-of-a-kind? [hand]
  (>= (amount-same-rank hand) 3))

(defn four-of-a-kind? [hand]
  (>= (amount-same-rank hand) 4))

(defn flush? [hand]
  (let [suits (map suit hand)
        suit (first suits)]
    (every? (fn [x] (= x suit))
            suits)))

(defn full-house? [hand]
  (= [2 3]
     (sort (card-counts hand))))

(defn two-pairs? [hand]
  (= [1 2 2]
     (sort (card-counts hand))))

(defn straight? [hand]
  (let [mod-vals (sort (map (fn [x] (mod (- x 1) 13))
                            (map rank hand)))
        lowest-val (first mod-vals)
        normalized-vals (map (fn [x] (- x lowest-val))
                             mod-vals)]
    (or (= [0 1 2 3 4] normalized-vals)
        (= [0 9 10 11 12] normalized-vals))))

(defn straight-flush? [hand]
  (and (flush? hand)
       (straight? hand)))

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
