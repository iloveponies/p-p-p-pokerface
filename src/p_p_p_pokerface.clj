(ns p-p-p-pokerface)

(def rank-map {\T 10, \J 11, \Q 12, \K 13, \A 14})
(def rank-map-sub {\T 10, \J 11, \Q 12, \K 13, \A 1})

(defn rank-with-map [card rmap]
  (let [[card-rank _] card]
    (if (Character/isDigit card-rank)
      (Integer/valueOf (str card-rank))
      (get rmap card-rank))))

(defn rank [card]
  (rank-with-map card rank-map))

(defn suit [card]
  (let [[_ card-suit] card]
    (str card-suit)))

(defn num-pairs [hand]
  (let [ranks (map rank hand)
        is-two? (fn [x] (= 2 x))]
    (count (filter is-two? (vals (frequencies ranks))))))

(defn pair? [hand]
    (= 1 (num-pairs hand)))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
        is-three? (fn [x] (= 3 x))]
    (= 1 (count (filter is-three? (vals (frequencies ranks)))))))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)
        is-four? (fn [x] (= 4 x))]
    (= 1 (count (filter is-four? (vals (frequencies ranks)))))))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (or (= 2 (num-pairs hand)) (four-of-a-kind? hand)))

(defn straight? [hand]
  (let [increasing-seq (fn [v] 
                         (range (first v) (+ (first v) (count v))))
        rank-sub (fn [x] (rank-with-map x rank-map-sub))
        rank-hand (sort (map rank hand))
        rank-sub-hand (sort (map rank-sub hand))]
    (or 
      (= (increasing-seq rank-hand) rank-hand)
      (= (increasing-seq rank-sub-hand) rank-sub-hand))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn value [hand]
  (cond
    (straight-flush? hand)  8
    (four-of-a-kind? hand)  7
    (full-house? hand)      6
    (flush? hand)           5
    (straight? hand)        4
    (three-of-a-kind? hand) 3
    (two-pairs? hand)       2
    (pair? hand)            1
    :else                   0))

