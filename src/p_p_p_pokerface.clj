(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card]
    (cond
      (Character/isDigit rank) (Integer/valueOf (str rank))
      :else (get {\A 14
                  \K 13
                  \Q 12
                  \J 11
                  \T 10}
                 rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
    (not (empty? (filter (fn [x] (= x 2)) (vals (frequencies (map rank hand)))))))

(defn three-of-a-kind? [hand]
  (not (empty? (filter (fn [x] (= x 3)) (vals (frequencies (map rank hand)))))))

(defn four-of-a-kind? [hand]
  (not (empty? (filter (fn [x] (= x 4)) (vals (frequencies (map rank hand)))))))

(defn flush? [hand]
  (= 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (= (seq [2 3]) (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (let [counts (sort (vals (frequencies (map rank hand))))]
    (or
      (= (seq [1 2 2]) counts)
      (= (seq [1 4]) counts))))

(defn straight? [hand]
  (let [values (sort (map rank hand))]
    (or
      (and
        (= 4 (- (last values) (first values)))
        (not (pair? hand)))
      (= (seq [2 3 4 5 14]) values))))

(defn straight-flush? [hand]
  (and
    (straight? hand)
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
