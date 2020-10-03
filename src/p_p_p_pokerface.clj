(ns p-p-p-pokerface)

(def suit-replacements {\T 10, \J 11 \Q 12 \K 13 \A 14})

(defn rank [card]
  (let [[rank _] card
         is-digit (Character/isDigit rank)
         rank->rank-string (cond
                             (= is-digit true)(Integer/valueOf (str rank))
                             :else (get suit-replacements rank))]
    rank->rank-string))


(defn suit [card]
  (let [[_ suit] card]
    (str suit)))


(defn rank-frequencies [hand]
  (frequencies(map(fn [card] (rank card)) hand)))


(defn suit-frequencies [hand]
  (frequencies(map(fn [card] (suit card)) hand)))


(defn pair? [hand]
  (let [is-pair? (fn [[_ x]] (= x 2))]
    (contains? (set (map is-pair? (rank-frequencies hand))) true)))


(defn three-of-a-kind? [hand]
  (let [is-three-of-a-kind? (fn [[_ x]] (= x 3))]
    (contains? (set (map is-three-of-a-kind? (rank-frequencies hand))) true)))


(defn four-of-a-kind? [hand]
  (let [is-four-of-a-kind? (fn [[_ x]] (= x 4))]
    (contains? (set (map is-four-of-a-kind? (rank-frequencies hand))) true)))


(defn flush? [hand]
  (let [is-flush? (fn [[_ x]] (= x 5))]
    (contains? (set (map is-flush? (suit-frequencies hand))) true)))


(defn full-house? [hand]
  (let [ranks (vec (map (fn [card] (rank card)) hand))]
    (= (sort (vals (frequencies ranks))) (seq [2 3]))))


(defn two-pairs? [hand]
  (or
    (= 2 (get (frequencies (vals (frequencies (map rank hand)))) 2))
    (= 1 (get (frequencies (vals (frequencies (map rank hand)))) 4))))


(defn straight? [hand]
  (let [sorted (sort (map rank hand))
        smallest (first sorted)
        alternative (sort (replace {14 1} sorted))
        alternative-smallest (first alternative)]
    (or
      (= sorted (range smallest (+ smallest 5)))
      (= alternative (range alternative-smallest (+ alternative-smallest 5))))))


(defn straight-flush? [hand]
  (and
    (straight? hand)
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
