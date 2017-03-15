(ns p-p-p-pokerface)

(def rankvals {\A 14, \K 13, \Q 12, \J 11, \T 10})

(defn rank [card]
(let [char (get card 0)]
  (if (Character/isDigit char) (Integer/valueOf (str char)) (rankvals char)
)))

(defn suit [card]
  (str (get card 1)
))

(defn ranks [hand]
  (map rank hand)
)

(defn suits [hand]
  (map suit hand)
)

(defn group-of? [hand num]
  (>= (apply max(vals(frequencies (ranks hand)))) num)
)

(defn pair? [hand] (group-of? hand 2))

(defn three-of-a-kind? [hand] (group-of? hand 3))

(defn four-of-a-kind? [hand] (group-of? hand 4))

(defn flush? [hand]
  (apply = (suits hand))
)

(defn full-house? [hand]
 (let [v (vals(frequencies (ranks hand)))]
  (or (= v [3 2]) (= v [2 3]))
))

(defn two-pairs? [hand]
 (let [v (sort (vals (frequencies (ranks hand))))]
  (or
   (= v [1 2 2])
   (full-house? hand)
   (four-of-a-kind? hand))
))

(defn straight? [hand]
 (let [minimi (apply min (ranks hand))]
  (or
   (= (map (fn[x] (- x minimi)) (sort (ranks hand))) (range 0 5))
   (= (sort (ranks hand)) [2 3 4 5 14]))
))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand))
)

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
    :else 0
))
