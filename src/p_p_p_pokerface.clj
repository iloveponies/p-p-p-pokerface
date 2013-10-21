(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card]
    (case r
      \T 10
      \J 11
      \Q 12
      \K 13
      \A 14
      (Integer/valueOf (str r)))))

(defn ranks [cards]
  (sort (map rank cards)))

(defn suit [card]
  (str (get card 1)))

(defn suits [cards]
  (map suit cards))

(defn rank_frequencies [cards]
  (sort (vals (frequencies (ranks cards)))))

(defn has_frequency [hand n]
  (boolean (some #{n} (rank_frequencies hand))))

(defn pair? [hand]
  (has_frequency hand 2))

(defn three-of-a-kind? [hand]
  (has_frequency hand 3))

(defn four-of-a-kind? [hand]
  (has_frequency hand 4))

(defn flush? [hand]
  (apply = (suits hand)))

(defn full-house? [hand]
  (= (rank_frequencies hand) [2, 3]))

(defn two-pairs? [hand]
  (or (= (rank_frequencies hand) [1, 2, 2]) (four-of-a-kind? hand)))

(defn is-straight? [cards]
  (every? (fn [[f s]] (= (- f s) 1)) (map vector (rest cards) (butlast cards))))

(defn straight? [hand]
  (let [h (ranks hand)]
    (or (is-straight? h) (is-straight? (sort (replace {14 1} h))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers [high-card? pair? two-pairs? three-of-a-kind? straight?
                  flush? full-house? four-of-a-kind? straight-flush?]]
    (apply max (map-indexed (fn [s f] (if (f hand) s 0)) checkers))))