(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank suit] card
        values {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit rank) 
      (Integer/valueOf (str rank))
      (get values rank))))

(defn suit [card]
  (let [[rank suit] card]
    (str suit)))

(defn pair? [hand]
  (let [f (fn [x] (> x 1))]
   	(not (empty? (filter f (vals (frequencies (map rank hand))))))))

(defn three-of-a-kind? [hand]
  (let [f (fn [x] (> x 2))]
   	(not (empty? (filter f (vals (frequencies (map rank hand))))))))

(defn four-of-a-kind? [hand]
  (let [f (fn [x] (> x 3))]
   	(not (empty? (filter f (vals (frequencies (map rank hand))))))))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (clojure.set/subset? 
   #{2 3} (set (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (let [f (fn [x] (> x 1))
        fs (filter f (vals (frequencies (map rank hand))))]
   	(or (> (count fs) 1)
        (contains? (set fs) 4))))

(defn straight? [hand]
  (let [sorted (sort (map rank hand))
        minval (first sorted)]
  (or (= sorted (range minval (+ 5 minval)))
      (= (sort (replace {14 1} sorted)) (range 1 6)))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn value [hand]
  (let [high-card? (fn [x] true)
        checkers #{[straight-flush? 8] [four-of-a-kind? 7]
                   [full-house? 6] [flush? 5] [straight? 4]
                   [three-of-a-kind? 3] [two-pairs? 2] 
                   [pair? 1] [high-card? 0]}
        f (fn [checker] [((first checker) hand) (second checker)])
        f_ (fn [x] (first x))]
    (last (sort (map second (filter f_ (map f checkers)))))))