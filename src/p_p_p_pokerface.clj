(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (get {\A 14, \K 13, \Q 12, \J 11, \T 10} fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (== 4 (count (set (mapv rank hand)))))

(defn three-of-a-kind? [hand]
  (and (== 3 (apply max (vals (frequencies (mapv rank hand))))) (== 3 (count (set (mapv rank hand))))))

(defn four-of-a-kind? [hand]
  (and (== 4 (apply max (vals (frequencies (mapv rank hand))))) (== 2 (count (set (mapv rank hand))))))

(defn flush? [hand]
  (apply = (mapv suit hand)))

(defn full-house? [hand]
  (and (== 3 (apply max (vals (frequencies (mapv rank hand))))) (== 2 (count (set (mapv rank hand))))))

(defn two-pairs? [hand]
  (and (== 2 (apply max (vals (frequencies (mapv rank hand))))) (== 3 (count (set (mapv rank hand))))))

(defn straight? [hand]
  (and (== 5 (count (set (mapv rank hand)))) (or (== 4 (- (apply max (map rank hand)) (apply min (map rank hand))))
      (== 4 (- (apply max (replace {14 1} (map rank hand))) (apply min (replace {14 1} (map rank hand))))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (cond (straight-flush? hand) 8
        (four-of-a-kind? hand) 7
        (full-house? hand) 6
        (flush? hand) 5
        (straight? hand) 4
        ( three-of-a-kind? hand) 3
        (two-pairs? hand) 2
        (pair? hand) 1
        :else 0))
