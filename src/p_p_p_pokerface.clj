(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card
        big {\T 10
             \J 11
             \Q 12
             \K 13
             \A 14}]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get big r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn high-card? [hand]
  (max (hand)))

(defn pair? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (apply = (keys (frequencies (map suit hand)))))

(defn full-house? [hand]
  (let [k (sort (vals (frequencies (map rank hand))))]
    (= [2 3] k)))

(defn two-pairs? [hand]
  (let [k (sort (vals (frequencies (map rank hand))))]
    (= [1 2 2] k)))

(defn straight? [hand]
  (let [h (map rank hand)
        h1 (map (fn [a] (if (= a 14) 1 a)) h)
        te (fn [hnd] (apply = (map - (sort hnd) [0 1 2 3 4])))]
    (or (te h) (te h1))))

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




