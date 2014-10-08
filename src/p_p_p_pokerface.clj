(ns p-p-p-pokerface)

(defn rank [card]
  (let [lookup {\T 10 \J 11 \Q 12 \K 13 \A 14}
        rank->int (fn [x]
                    (if (Character/isDigit x) (Integer/valueOf (str x))))
        valid? (fn [x]
                 (cond (contains? lookup x) (get lookup x)
                       :else (rank->int x)))]
    (valid? (first card))))

(defn suit [card]
  (str (second card)))

(defn rank-freqs [hand]
  (let [ranksh (map rank hand)
        freq (frequencies ranksh)]
    (sort (vals freq))))

(defn pair? [hand]
  (let [rank-freqs (rank-freqs hand)
        pairs (filter (fn [x] (>= x 2)) rank-freqs)]
    (not (empty? pairs))))

(defn three-of-a-kind? [hand]
  (let [rank-freqs (rank-freqs hand)
        pairs (filter (fn [x] (>= x 3)) rank-freqs)]
    (not (empty? pairs))))

(defn four-of-a-kind? [hand]
  (let [rank-freqs (rank-freqs hand)
        pairs (filter (fn [x] (>= x 4)) rank-freqs)]
    (not (empty? pairs))))

(defn flush? [hand]
  (let [suits (map suit hand)
        freq (frequencies suits)
        vals (vals freq)
        flush (filter (fn [x] (= x 5)) vals)]
    (not (empty? flush))))

(defn full-house? [hand]
  (let [rank-freqs (rank-freqs hand)]
    (= [2 3] rank-freqs)))

(defn two-pairs? [hand]
  (let [rank-freqs (rank-freqs hand)
        pair2 (filter (fn [x] (= x 2)) rank-freqs)
        pair4 (some (fn [x] (= x 4)) rank-freqs)]
    (or (>= (count pair2) 2) pair4 false)))

(defn straight? [hand]
  (let [
        ranks (sort (map rank hand))
        mapAce {14 1}
        altranks (sort (replace mapAce ranks))
        consec (fn [[x & xs]]
                 ; define recursive function that checks if the
                 ; elements of a seq are consecutive
                 (if (not xs) true
                 (and
                  (= (- (first xs) 1) x)
                  (recur xs))))]
    (or (consec ranks) (consec altranks))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn value [hand]
  nil)
