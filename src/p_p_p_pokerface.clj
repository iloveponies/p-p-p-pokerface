(ns p-p-p-pokerface)

(defn rank [card]
  (let [replacements {\A 14, \K 13, \Q 12, \J 11, \T 10}
        [fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (replacements fst)
      )))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (>= (apply max(vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (>= (apply max(vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (= (apply max(vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (= (apply max(vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (and
   (= (apply max(vals (frequencies (map rank hand)))) 3)
   (= (apply min(vals (frequencies (map rank hand)))) 2)))

(defn two-pairs? [hand]
  nil)

(defn straight? [hand]
  (let [minrank (apply min(map rank hand))]
  (if (or
       (=
        (sort (map rank hand))
        (range minrank (+ minrank 5)))
       (=
        (sort (map rank hand))
        (sort [2 3 4 5 14])))
    true
    false)))

(defn straight-flush? [hand]
  (if (and
       (straight? hand)
       (flush? hand))
  true
  false))

(defn value [hand]
  nil)
