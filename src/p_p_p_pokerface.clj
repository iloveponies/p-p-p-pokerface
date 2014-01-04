(ns p-p-p-pokerface)

(defn suit [card]
  (let [[_ b] card]
    (str b)))

(def rank-names {
  \T 10, \J 11, \Q 12,
  \K 13, \A 14 })

(defn rank [card]
  (let [[a _] card]
    (if (Character/isDigit a)
      (Integer/valueOf (str a))
      (get rank-names a))))

(defn rank-frequencies [hand]
  (vals (frequencies (map rank hand))))

(defn has-height? [hand height]
  (> (.indexOf (rank-frequencies hand) height) -1))

(defn pair? [hand]
  (has-height? hand 2))

(defn three-of-a-kind? [hand]
  (has-height? hand 3))

(defn four-of-a-kind? [hand]
  (has-height? hand 4))

(defn flush? [hand]
  (== (apply max (vals (frequencies
    (map suit hand)))) 5))

(defn full-house? [hand]
  (and
    (pair? hand)
    (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (== (count (filter #{2}
    (rank-frequencies hand))) 2))

(defn straight? [hand]
  (let [sorted (sort (map rank hand))]
    (or (= [2 3 4 5 14] sorted)
      (and
        (apply < sorted)
        (== (last sorted) (+ 4 (first sorted)))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(def hand-values #{
  [high-card? 0]
  [pair? 1]
  [two-pairs? 2]
  [three-of-a-kind? 3]
  [straight? 4]
  [flush? 5]
  [full-house? 6]
  [four-of-a-kind? 7]
  [straight-flush? 8]})

(defn value [hand]
  (let [kinds (filter #((first %) hand) hand-values)]
    (apply max (map second kinds))))
