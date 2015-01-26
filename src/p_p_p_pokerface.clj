(ns p-p-p-pokerface)

(defn rank
  ([[card-rank _]]
   (rank false [card-rank _]))
  ([ace-low [rank _]]
   (if (Character/isDigit rank)
     (Integer/valueOf (str rank))
     ({\T 10 \J 11 \Q 12 \K 13 \A (if ace-low 1 14)} rank))))

(defn suit [[_ suit]]
  (str suit))

(defn pair? [hand]
  (not= 5 (count (set (map rank hand)))))

(defn- rank-frequencies [hand]
  (vals (frequencies (map rank hand))))

(defn- n-of-a-kind [n hand]
  (boolean (some (partial <= n) (rank-frequencies hand))))

(defn three-of-a-kind? [hand]
  (n-of-a-kind 3 hand))

(defn four-of-a-kind? [hand]
  (n-of-a-kind 4 hand))

(defn flush? [hand]
  (= 1 (count (set (map suit hand)))))

(defn full-house? [hand]
  (= #{2 3} (set (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (>= 1 (get (frequencies (rank-frequencies hand)) 1 0)))

(defn straight? [hand]
  (letfn [(straight? [ranks]
                     (let [min-rank (apply min ranks)]
                       (=
                        (range min-rank (+ min-rank (count ranks)))
                        (sort ranks))))]
    (or
     (straight? (map (partial rank false) hand))
     (straight? (map (partial rank true) hand)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [matchers [pair? two-pairs? three-of-a-kind? straight? flush?
                  full-house? four-of-a-kind? straight-flush?]
        hand-values (map inc (range (count matchers)))
        value-for-match (fn [matcher value] (if (matcher hand) value 0))]
    (apply max (map value-for-match matchers hand-values))))
