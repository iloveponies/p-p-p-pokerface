(ns p-p-p-pokerface)



(defn rank [card]
  (def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})
  (let [[r _] card]
  (if (nil? (get replacements r))
    (Integer/valueOf (str r))
    (get replacements r))))



(defn suit
  [card]
  (let [[_ s] card]
    (str s)))




(defn pair?
  [hand]
  (= [1 1 1 2] (vec (sort (vals (frequencies (map rank hand)))))))



(defn three-of-a-kind?
  [hand]
  (= [1 1 3] (vec (sort (vals (frequencies (map rank hand)))))))

(defn four-of-a-kind?
  [hand]
  (= [1 4] (vec (sort (vals (frequencies (map rank hand)))))))

(defn flush?
  [hand]
  (if (= (count (set (vals (frequencies (map suit hand))))) 1)
    true
    false))


(defn full-house?
  [hand]
  (if (= (set (vals (frequencies (map rank hand)))) #{2 3})
      true
      false))

(defn two-pairs?
  [hand]
  (= [1 2 2] (vec (sort (vals (frequencies (map rank hand)))))))

(defn straight?
  [hand]
  (or (= (sort (map rank hand)) (range (first (sort (map rank hand))) (+ 5 (first (sort (map rank hand))))))
      (= (sort (replace {14 1} (map rank hand))) (range (first (sort (replace {14 1} (map rank hand)))) (+ 5 (first (sort (replace {14 1} (map rank hand)))))))))



(defn straight-flush?
  [hand]
  (and (straight? hand) (flush? hand)))


(defn value
  [hand]
  (cond (pair? hand) 1
        (two-pairs? hand) 2
        (straight-flush? hand) 8
        (three-of-a-kind? hand) 3
        (straight? hand) 4
        (flush? hand) 5
        (full-house? hand) 6
        (four-of-a-kind? hand) 7

        :else 0))

