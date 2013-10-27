(ns p-p-p-pokerface)

(def rank-mappings
  (into {}
        (map vector
             (concat (range (int \2) (inc (int \9)))
                     (map int [\T \J \Q \K \A]))
             (concat (range 2 (inc 14))))))

(defn rank
  [card]
  (rank-mappings (int (first card))))

(defn suit
  [card]
  (str (second card)))

(defn- rank-freqs
  [hand]
  (sort (vals (frequencies (map first hand)))))

(defn pair?
  [hand]
  (= [1 1 1 2] (rank-freqs hand)))

(defn three-of-a-kind?
  [hand]
  (= [1 1 3] (rank-freqs hand)))

(defn four-of-a-kind?
  [hand]
  (= [1 4] (rank-freqs hand)))

(defn flush?
  [hand]
  (apply = (map second hand)))

(defn full-house?
  [hand]
  (= [2 3] (rank-freqs hand)))

(defn two-pairs?
  [hand]
  (= [1 2 2] (rank-freqs hand)))

(defn straight?
  [hand]
  (let [straight-hands (set (map set (partition 5 1 [\A \2 \3 \4 \5
                                                     \6 \7 \8 \9 \T
                                                     \J \Q \K \A])))]
    (contains? straight-hands (set (map first hand)))))

(defn straight-flush?
  [hand]
  (and (straight? hand) (flush? hand)))

(def scoring
  (map vector
       (range 8 (dec 0) -1)
       [straight-flush? four-of-a-kind? full-house?
        flush? straight? three-of-a-kind? two-pairs? pair?
        (constantly true)]))

(defn value
  [hand]
  (ffirst (filter (fn [[_ pred]]
                    (pred hand))
                  scoring)))

