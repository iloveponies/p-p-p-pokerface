(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r s] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get {\T 10, \J 11, \Q 12, \K 13, \A 14} r))))

(defn suit [card]
  (let [[r s] card]
    (str s)))

(defn- sorted-rank-freqs [hand]
  (sort (vals (frequencies (map rank hand)))))

(defn- n-of-a-kind? [hand n]
  (boolean (some #(>= % n) (sorted-rank-freqs hand))))

(defn pair? [hand]
  (n-of-a-kind? hand 2))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? hand 3))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? hand 4))

(defn flush? [hand]
  (= 1 (count (set (map suit hand)))))

(defn full-house? [hand]
  (= [2 3] (sorted-rank-freqs hand)))

(defn two-pairs? [hand]
  (= [1 2 2] (sorted-rank-freqs hand)))

(defn straight? [hand]
  (let [sorted (sort (map rank hand))
        fst    (first sorted)
        diffs  (map #(- % fst) sorted)]
    (or (= [0 1 2 3 4] diffs) (= [2 3 4 5 14] sorted))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [choices [(fn [x] true) pair? two-pairs? three-of-a-kind?
                 straight? flush? full-house? four-of-a-kind? straight-flush?]]
    (last (filter identity (map #(if (%2 hand) %1 nil) (range) choices)))))
