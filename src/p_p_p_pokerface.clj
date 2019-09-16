(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card]
    (cond
      (= rank \T) 10
      (= rank \J) 11
      (= rank \Q) 12
      (= rank \K) 13
      (= rank \A) 14
      :else       (Integer/valueOf (str rank)))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn rep-cards? [hand n]
  (= n (apply max (vals (frequencies (map rank hand))))))

(defn pair? [hand]
  (rep-cards? hand 2))

(defn three-of-a-kind? [hand]
  (rep-cards? hand 3))

(defn four-of-a-kind? [hand]
  (rep-cards? hand 4))

(defn flush? [hand]
  (= 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (let [values (set (vals (frequencies (map rank hand))))]
    (= #{2 3} values)))

(defn _two-pairs? [hand]
  (let [values (sort (vals (frequencies (map rank hand))))]
    (= (list 1 2 2) values)))

(defn two-pairs? [hand]
  (or (four-of-a-kind? hand)
      (_two-pairs? hand)))

(defn _straight? [ranks]
  (let [values (sort (set ranks))]
    (and (= (count values) 5)
         (= (- (last values) (first values)) 4))))

(defn straight? [hand]
  (let [ranks (map rank hand)]
    (or (_straight? (replace {1 14} ranks))
        (_straight? (replace {14 1} ranks)))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

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

