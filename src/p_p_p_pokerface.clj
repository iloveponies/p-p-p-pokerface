(ns p-p-p-pokerface)

(defn rank [card]
  (def replacements {\T 10, \J 11, \Q 12,
                     \K 13, \A 14})
  (let [[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (replacements rank))))

(defn suit [card]
  (let [[_ suit] card]
  (str suit)))

(defn pair? [hand]
  (if (= (apply max (vals (frequencies (map rank hand)))) 2)
    true false))

(defn three-of-a-kind? [hand]
  (if (= (apply max (vals (frequencies (map rank hand)))) 3)
    true false))

(defn four-of-a-kind? [hand]
  (if (= (apply max (vals (frequencies (map rank hand)))) 4)
    true false))

(defn flush? [hand]
  (if (= (apply max (vals (frequencies (map suit hand)))) 5)
    true false))

(defn full-house? [hand]
  (if (and (three-of-a-kind? hand) (= (apply min (vals (frequencies (map rank hand)))) 2))
    true false))

(defn two-pairs? [hand]
  (cond
   (four-of-a-kind? hand) true
   (and (not (three-of-a-kind? hand))
        (= (count (vals (frequencies (map rank hand)))) 3))
        true
   :else false))

(defn straight? [hand]
  (let [sorted (sort (map rank hand))
        rep (sort (replace {14 1} sorted))
        sta1 (apply min sorted)
        sta2 (apply min rep)]
    (if (or
         (= sorted (range sta1 (+ sta1 5)))
         (= rep (range sta2 (+ sta2 5))))
      true false)))

(defn straight-flush? [hand]
  (if (and (straight? hand) (flush? hand)) true false))

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

