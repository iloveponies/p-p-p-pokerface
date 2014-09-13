(ns p-p-p-pokerface)

(defn rank [card]
  (let [[s _]
        card]
    (cond
     (Character/isDigit s) (Integer/valueOf (str s))
     :else ({\T 10, \J 11, \Q 12, \K 13, \A 14} s))))

(defn suit [card]
  (let [[_ r]
        card]
    (str r)))

(defn hand-rank [hand]
  (vals(frequencies (map rank hand))))

(defn pair? [hand]
  (cond
   (contains? (set(hand-rank hand)) 4) true
   (contains? (set(hand-rank hand)) 3) true
   (contains? (set(hand-rank hand)) 2) true
   :else false))

(defn three-of-a-kind? [hand]
  (cond
   (contains? (set(hand-rank hand)) 3) true
   (contains? (set(hand-rank hand)) 4) true
   :else false))

(defn four-of-a-kind? [hand]
  (contains? (set(hand-rank hand)) 4))

(defn flush? [hand]
  (contains? (set(vals(frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (= [2 3] (sort (hand-rank hand))))

(defn two-pairs? [hand]
  (= [1 2 2] (sort (hand-rank hand))))

(defn straight? [hand]
  (let [a-to-one (fn [x] (sort(replace {14 1} (mapv rank hand))))]
    (cond
     (== (apply min(mapv rank hand)) 2) (= (a-to-one hand) (range (apply min (a-to-one hand)) (+ (apply min (a-to-one hand)) 5)))
     :else (= (sort(mapv rank hand)) (range (apply min (mapv rank hand)) (+ (apply min (mapv rank hand)) 5))))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))


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
