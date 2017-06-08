(ns p-p-p-pokerface)

(def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[fst snd] card]
  (if (Character/isDigit fst)
    (Integer/valueOf (str fst))
    (replacements fst))))

(defn suit [card]
  (let [[_ snd] card]
  (str snd)))

(defn pair? [hand]
 (>= (apply max (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
   (= (apply max (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (and (= (apply min (vals (frequencies (map rank hand)))) 2) (= (apply max (vals (frequencies (map rank hand)))) 3)))

(defn two-pairs? [hand]
  (or (= (sort (vals (frequencies (map rank hand)))) [1 2 2]) (four-of-a-kind? hand)))

(defn straight? [hand]
  (if (= (apply min (map rank hand)) 2)
    (= (sort (replace {14 1} (map rank hand))) (range (apply min (replace {14 1} (map rank hand))) (+ (apply max (replace {14 1} (map rank hand))) 1)))
    (= (sort (map rank hand)) (range (apply min (map rank hand)) (+ (apply max (map rank hand))1)))))

(defn straight-flush? [hand]
 (and (straight? hand) (flush? hand)))


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
