(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst snd] card]
    (if (Character/isDigit fst)
       (Integer/valueOf (str fst))
       (get {\T 10, \J 11,
             \Q 12, \K 13,
             \A 14,} fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (= (apply max (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (= (apply max (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (= (apply max (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
(and (= (apply max (vals (frequencies (map suit hand)))) 5)
     (= (apply max (vals (frequencies (map rank hand)))) 1)))

(defn full-house? [hand]
(and (= (apply max (vals (frequencies (map rank hand)))) 3)
     (= (apply min (vals (frequencies (map rank hand)))) 2)))

(defn two-pairs? [hand]
  (or (= (sort (vals (frequencies (map rank hand))))
           [1 2 2])
      (= (sort (vals (frequencies (map rank hand))))
           [1 4])))

(defn straight? [hand]
  (let [aceslow  (sort (replace {14 1} (map rank hand)))
        aceshigh (sort (map rank hand))
        minimum  (first (map rank hand))]
  (or (or (= aceshigh [10 11 12 13 14])
          (= aceslow  [1 2 3 4 5]))
      (= (sort (map rank hand))
           (range minimum (+ minimum 5))))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  ;eiks tää oo se helppolukuisin vaihtoehto?
  (cond
    (straight-flush? hand) 8
    (four-of-a-kind? hand) 7
    (full-house? hand)     6
    (flush? hand)          5
    (straight? hand)       4
    (three-of-a-kind? hand)3
    (two-pairs? hand)      2
    (pair? hand)           1
    (high-card? hand)      0))




