(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card]
    (cond (= fst \1) 10
          (Character/isDigit fst) (Integer/valueOf (str fst))
          :else ( get {\A 14, \K 13, \Q 12, \J 11, \T 10} fst))
    ))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 2)
  )

(defn three-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 3)
  )

(defn four-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 4)
  )

(defn flush? [hand]
  (= (apply max (vals (frequencies (map suit hand)))) 5)
  )

(defn full-house? [hand]
  (let [v (sort (vals (frequencies (map rank hand))))]
    (= '(2 3) v)
    ))

(defn two-pairs? [hand]
  (let [v (sort (vals(frequencies (map rank hand))))]
    (= '(1 2 2) v)
    ))

(defn straight? [hand]
  (let [sorted (sort (map rank hand))
        lowest (first sorted)
        altsorted (sort (replace {14 1} sorted))]
    (or (= sorted (range lowest (+ lowest 5)))
        (= altsorted (range 1 6)))))

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
