(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card
        rank->value {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (rank->value r))))

(defn suit [card]
  (str (second card)))

(defn pair? [hand]
  (< 0 (count (filter (fn [x] (= x 2)) (vals (frequencies (map rank hand)))))))

(defn three-of-a-kind? [hand]
  (< 0 (count (filter (fn [x] (= x 3)) (vals (frequencies (map rank hand)))))))

(defn four-of-a-kind? [hand]
  (< 0 (count (filter (fn [x] (= x 4)) (vals (frequencies (map rank hand)))))))

(defn flush? [hand]
  (< 0 (count (filter (fn [x] (= x 5)) (vals (frequencies (map suit hand)))))))

(defn full-house? [hand]
  (and (three-of-a-kind? hand)
       (pair? hand)))

(defn two-pairs? [hand]
  (= 2 (count (filter (fn [x] (= x 2)) (vals (frequencies (map rank hand)))))))

(defn straight? [hand]
  (let [hand-a (sort (map rank hand))
       hand-b (sort (replace {14 1} hand-a))]
    (or (= (range (apply min hand-a) (+ 1 (apply max hand-a))) hand-a)
        (= (range (apply min hand-b) (+ 1 (apply max hand-b))) hand-b))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

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
