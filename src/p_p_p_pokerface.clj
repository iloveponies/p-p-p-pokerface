(ns p-p-p-pokerface)

(defn rank [card]
  (let[[fst snd] card]
    snd))

(defn suit [card]
  (let [[fst snd] card]
    fst))

(defn pair? [hand]
  (let [[fst snd] (first hand)
        [fst1 snd1] (second hand)]
  (= fst fst1)))

(defn three-of-a-kind? [hand]
  (= (first (vals (frequencies (map #(first %) hand)))) 3))

(defn four-of-a-kind? [hand]
  (= (first (vals (frequencies (map #(first %) hand)))) 4))

(defn flush? [hand]
  (= (first (vals (frequencies (map #(second %) hand)))) 5))

(defn full-house? [hand]
  (and
  (= (first (vals (sort (frequencies (map #(first %) hand))))) 3)
  (= (second (vals (sort (frequencies (map #(first %) hand))))) 2)))

(defn two-pairs? [hand]
  (or (= (+
    (first (vals (sort (frequencies (map #(first %) hand)))))
    (second (vals (sort (frequencies (map #(first %) hand)))))) 4)
      (= (first (vals (sort (frequencies (map #(first %) hand))))) 4)))

(defn straight? [hand]
  (let [sorted (sort (map first hand))
        ace (if (= (apply min (map int sorted)) 50) (sort (map int (replace {\A 49} sorted))) (sort (map int (replace {\T 58, \J 59, \Q 60, \K 61 \A 62} sorted))))
        min-value (apply min (map int ace))]
    (= ace (take 5 (iterate inc min-value)))))

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
