(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r s] card ]
    (if (Character/isDigit r)
        (Integer/valueOf (str r))
        (cond
            (= r \T) 10
            (= r \J) 11
            (= r \Q) 12
            (= r \K) 13
            (= r \A) 14
            ))))

(defn suit [card]
  (let [[r s] card]
    (str s)))

(defn pair? [hand]
  (let [f (frequencies (map rank hand))
        amounts (set (vals f))]
    (contains? amounts 2)))

(defn three-of-a-kind? [hand]
  (let [f (frequencies (map rank hand))
        amounts (set (vals f))]
    (contains? amounts 3)))

(defn four-of-a-kind? [hand]
  (let [f (frequencies (map rank hand))
        amounts (set (vals f))]
    (contains? amounts 4)))

(defn flush? [hand]
  (= 1 (count (keys (frequencies (map suit hand))))))

(defn full-house? [hand]
  (let [f (frequencies (map rank hand))
        amounts (set (vals f))]
    (and (contains? amounts 3) (contains? amounts 2))))

(defn two-pairs? [hand]
  (let [f (frequencies (map rank hand))
        amounts (vals f)]
    (= 2 (count (filter (fn [x] (= x 2)) amounts)))))

(defn straight? [hand]
    (let [ranks (sort (map rank hand))
          ranks-ace-as-one (sort (replace {14 1} ranks))
          straight-ranks? (fn [ranks]
            (let [[fst] ranks]
                (= (range fst (+ 5 fst)) ranks)))]
        (or (straight-ranks? ranks) (straight-ranks? ranks-ace-as-one))
      ))

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
    :else 0
    ))
