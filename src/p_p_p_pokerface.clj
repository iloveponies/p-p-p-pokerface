(ns p-p-p-pokerface)

(defn rank [[rank _]]
  ({\2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9 \T 10 \J 11 \Q 12 \K 13 \A 14} rank))

(defn suit [[_ suit]]
  (str suit))

(defn- freq-vals [* f hand]
  (->> hand (map *) frequencies vals f))

(defn- one-*-max-card-count [* hand]
  (freq-vals * (partial apply max) hand))

(def ^:private one-rank-max-card-count (partial one-*-max-card-count rank))

(def ^:private one-suit-max-card-count (partial one-*-max-card-count suit))

(defn pair? [hand]
  (== (one-rank-max-card-count hand) 2))

(defn three-of-a-kind? [hand]
  (== (one-rank-max-card-count hand) 3))

(defn four-of-a-kind? [hand]
  (== (one-rank-max-card-count hand) 4))

(defn flush? [hand]
  (== (one-suit-max-card-count hand) 5))

(defn full-house? [hand]
  (= (freq-vals rank sort hand) [2 3]))

(defn two-pairs? [hand]
  (= (freq-vals rank sort hand) [1 2 2]))

(defn- -straight? [replacements hand]
  (let [ranks (->> hand (map rank) (replace replacements) sort)
        rank-1 (first ranks)
        sample (take 5 (range rank-1 15))]
    (= ranks sample)))

(defn straight? [hand]
  (or (-straight? {} hand)
      (-straight? {14 1} hand)))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

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
