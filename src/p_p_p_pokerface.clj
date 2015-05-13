(ns p-p-p-pokerface)

(defn rank [card]
    (if (Character/isDigit (get card 0))
      (Integer/valueOf (str (get card 0)))
        (get {\T 10, \J 11, \Q 12, \K 13, \A 14} (get card 0))))

(defn suit [card]
  (str (get card 1)))

(defn pair? [hand]
  (if (= 2 (apply max (vals (frequencies (map rank hand))))) true false))

(defn three-of-a-kind? [hand]
  (if (= 3 (apply max (vals (frequencies (map rank hand))))) true false))

(defn four-of-a-kind? [hand]
  (if (= 4 (apply max (vals (frequencies (map rank hand))))) true false))

(defn flush? [hand]
  (let [vari (suit (first hand))]
    (if (= 4 (count (filter (fn [x] (= (suit x) vari)) (rest hand)))) true false)))

(defn full-house? [hand]
  (let [min (apply min (vals (frequencies (map rank hand))))
        max (apply max (vals (frequencies (map rank hand))))
        tulos (if (and (= 2 min) (= 3 max)) true false)]
    tulos))

(defn two-pairs? [hand]
  (let [max (apply max (vals (frequencies (map rank hand))))
        min (apply min (vals (frequencies (map rank hand))))
        count (count (vals (frequencies (map rank hand))))
        tulos (if (and (= 1 min) (= 2 max) (= 3 count)) true false)]
    tulos))

(defn straight? [hand]
  (let [min (apply min(map rank hand))
        uusi (range min (+ min 5))
        uusi2 (sort [2 3 4 5 14])
        jarjestetty (sort (map rank hand))]
    (if (or (= uusi jarjestetty) (= uusi2 jarjestetty)) true false)))

(defn straight-flush? [hand]
  (if (and (flush? hand) (straight? hand)) true false))

(defn value [hand]
  (cond (straight-flush? hand) 8
        (four-of-a-kind? hand) 7
        (full-house? hand) 6
        (flush? hand) 5
        (straight? hand) 4
        (three-of-a-kind? hand) 3
        (two-pairs? hand) 2
        (pair? hand) 1
        :else 0))



