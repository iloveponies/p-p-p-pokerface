(ns p-p-p-pokerface)

(defn rank [card]
  (let [[arvo _] card]
    (if (Character/isDigit arvo)
      (Integer/valueOf (str arvo))
      (get {\A 14, \K 13, \Q 12, \J 11, \T 10} arvo) )))

(defn suit [card]
  (let [[_ maa] card]
    (str maa)))

(defn pair? [hand]
  (if (== (apply max (vals (frequencies (map rank hand)))) 2)
    true
    false))

(defn three-of-a-kind? [hand]
  (if (== (apply max (vals (frequencies (map rank hand)))) 3)
    true
    false))

(defn four-of-a-kind? [hand]
  (if (== (apply max (vals (frequencies (map rank hand)))) 4)
    true
    false))

(defn flush? [hand]
  (if (== (apply max (vals (frequencies (map suit hand)))) 5)
  true
  false))

(defn full-house? [hand]
   (if (and (== (apply max (vals (frequencies (map rank hand)))) 3)
            (== (apply min (vals (frequencies (map rank hand)))) 2))
    true
    false))

(defn two-pairs? [hand]
  (if (or (= (vals (frequencies (map rank hand))) [2 2 1])
          (= (vals (frequencies (map rank hand))) [2 1 2])
          (= (vals (frequencies (map rank hand))) [1 2 2])
          (four-of-a-kind? hand))
    true
    false))

(defn straight? [hand]
  (let [pienin (apply min (keys (frequencies (map rank hand))))
        suurin (apply max (keys (frequencies (map rank hand))))
        jarjestetty-kasi (sort (keys (frequencies (map rank hand))))]
    (if (and (== pienin 2) (== suurin 14))
      (if (=  (keys (frequencies (map rank hand))) [2 3 4 5 14])
        true
        false)
      (if (= jarjestetty-kasi
             (range pienin (+ pienin 5)))
        true
        false))))

(defn straight-flush? [hand]
  (if (and (flush? hand) (straight? hand))
    true
    false))

(defn value [hand]
  nil)