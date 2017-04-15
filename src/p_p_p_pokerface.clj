(ns p-p-p-pokerface)

(def high-cards {\A 14, \K 13, \Q 12, \J 11, \T 10})

(defn rank [card]
  (let [[r _] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get high-cards r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn has-number-of-rank? [number hand]
  (= number (apply max (vals (frequencies (map rank hand))))))

(defn pair? [hand]
  (has-number-of-rank? 2 hand))

(defn three-of-a-kind? [hand]
  (has-number-of-rank? 3 hand))

(defn four-of-a-kind? [hand]
  (has-number-of-rank? 4 hand))

(defn flush? [hand]
  (= 5 (apply max (vals (frequencies (map suit hand))))))

(defn has-combinations? [comb1 comb2 hand]
  (let [sorted (sort hand)]
    (or (and (comb1 (take 3 sorted)) (comb2 (take-last 2 sorted)))
        (and (comb1 (take-last 3 sorted)) (comb2 (take 2 sorted))))))

(defn full-house? [hand]
  (has-combinations? three-of-a-kind? pair? hand))

(defn two-pairs? [hand]
  (has-combinations? pair? pair? hand))

(defn has-straight? [hand]
  (let [head (Integer/valueOf (first hand))]
    (= hand (range head (+ head 5)))))

(defn straight? [hand]
  (let [ranks (sort (replace high-cards (map rank hand)))
        ranks-low-ace (sort (replace {14 1} ranks))]
    (or (has-straight? ranks) (has-straight? ranks-low-ace))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn highest-value? [hand]
  (let [checkers [high-card? pair? two-pairs? three-of-a-kind? straight?
                  flush? full-house? four-of-a-kind? straight-flush?]
        check-map (map-indexed vector checkers)]
    (apply max (map first (filter (fn [v] ((second v) hand)) check-map)))))

(defn value [hand]
  (highest-value? hand))
