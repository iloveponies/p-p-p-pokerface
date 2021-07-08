(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get {\T 10, \J 11, \Q 12, \K 13, \A 14} r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn highest-freq-rank [hand]
  (apply max (vals (frequencies (map rank hand)))))

(defn highest-freq-suit [hand]
  (apply max (vals (frequencies (map suit hand)))))

(defn kind-count [hand n]
  (let [m (highest-freq-rank hand)]
    (if (= m n) true false)))

(defn pair? [hand]
  (kind-count hand 2))

(defn three-of-a-kind? [hand]
  (kind-count hand 3))

(defn four-of-a-kind? [hand]
  (kind-count hand 4))

(defn flush? [hand]
  (let [m (highest-freq-suit hand)]
    (if (= m 5) true false)))

(defn full-house? [hand]
  (= (sort (vals (frequencies (map rank hand)))) (seq [2 3])))

(defn two-pairs? [hand]
  (let [c (sort (vals (frequencies (map rank hand))))]
    (or (= c (seq [1 2 2])) (= c (seq [1 4])))))

(defn straight? [hand]
  (let [sorted-ranks
        (if (= (first (sort (map rank hand))) 2)
          (sort (replace {14 1} (sort (map rank hand)))) (sort (map rank hand)))]
  (if (= sorted-ranks (range (first sorted-ranks) (+ (first sorted-ranks) 5))) true false)))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [values ((juxt high-card? pair?
        two-pairs? three-of-a-kind?
        straight? flush? full-house?
        four-of-a-kind? straight-flush?) hand)]
    (apply max (map first (filter second (map-indexed vector values))))))
