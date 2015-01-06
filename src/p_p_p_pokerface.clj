(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card]
  (if (Character/isDigit fst)
    (Integer/valueOf (str fst))
    (cond
      (= fst \T) 10
      (= fst \J) 11
      (= fst \Q) 12
      (= fst \K) 13
      (= fst \A) 14 ))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn rank-frequencies [hand]
  (sort (vals (frequencies (map rank hand)))))

(defn highest-frequency [hand]
  (apply max (rank-frequencies hand)))

(defn pair? [hand]
  (= (highest-frequency hand) 2))

(defn three-of-a-kind? [hand]
  (= (highest-frequency hand) 3))

(defn four-of-a-kind? [hand]
  (= (highest-frequency hand) 4))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (= (rank-frequencies hand) [2 3]))

(defn two-pairs? [hand]
  (or (= (rank-frequencies hand) [1 2 2])
      (= (rank-frequencies hand) [1 4])))

(defn straight? [hand]
  (let [aces-high       (map rank hand)
        aces-low        (replace {14 1} (map rank hand))
        minimise        (fn [h] (sort (map #(- % (- (apply min h) 1)) h)))
        run-is-straight (fn [h] (= (minimise h) [1 2 3 4 5]))]
  (true? (some run-is-straight [aces-high aces-low]))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (cond
    (straight-flush? hand)  8
    (four-of-a-kind? hand)  7
    (full-house? hand)      6
    (flush? hand)           5
    (straight? hand)        4
    (three-of-a-kind? hand) 3
    (two-pairs? hand)       2
    (pair? hand)            1
    :else                   0))
