(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (cond
        (= r \T) 10
        (= r \J) 11
        (= r \Q) 12
        (= r \K) 13
        (= r \A) 14))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn rankvals [hand]
  (vals (frequencies (map rank hand))))

(defn suitvals [hand]
  (vals (frequencies (map suit hand))))

(defn conts [lst vl]
  (if (= nil (some #(= vl %) lst))
    false
    true))

(defn contr [hand r]
  (conts (rankvals hand) r))

(defn pair? [hand]
  (contr hand 2))

(defn three-of-a-kind? [hand]
  (contr hand 3))

(defn four-of-a-kind? [hand]
  (contr hand 4))

(defn flush? [hand]
  (= 1 (count (suitvals hand))))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [rh (rankvals hand)
        rfreqs (frequencies rh)]
    (or (four-of-a-kind? hand) (if (contains? rfreqs 2)
                                 (= 2 (get rfreqs 2))
                                 false))))

(defn straight? [hand]
  (let [rh (map rank hand)
        rrh (replace {14 1} (map rank hand))
        rm (apply min rh)
        rrm (apply min rrh)
        rrng (range rm (+ rm 5))
        rrrng (range rrm (+ rrm 5))]
    (or (= rrng (sort rh)) (= rrrng (sort rrh)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (cond (straight-flush? hand) 8
        (four-of-a-kind? hand) 7
        (full-house? hand) 6
        (flush? hand) 5
        (straight? hand) 4
        (three-of-a-kind? hand) 3
        (two-pairs? hand) 2
        (pair? hand) 1
        :else 0))
