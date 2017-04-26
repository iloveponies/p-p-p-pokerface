(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card
        ranks {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (get ranks rank))))

(defn suit [card]
  (let [[_ suit] card]
    (if (contains? #{\C \H \S \D} suit)
      (str suit)
      nil)))

(defn pair? [hand]
  (let [n-ranks (apply max (vals (frequencies (map rank hand))))]
    (= 2 n-ranks)))

(defn three-of-a-kind? [hand]
  (let [n-ranks (apply max (vals (frequencies (map rank hand))))]
    (= 3 n-ranks)))

(defn four-of-a-kind? [hand]
  (let [n-ranks (apply max (vals (frequencies (map rank hand))))]
    (= 4 n-ranks)))

(defn flush? [hand]
  (let [n-suits (count (set (map suit hand)))]
    (= n-suits 1)))

(defn full-house? [hand]
  (let [freq-set (sort (vals (frequencies (map rank hand))))]
    (= [2 3] freq-set)))

(defn two-pairs? [hand]
  (let [not-one (fn [x] (> x 1))
        ranks (vals (frequencies (map rank hand)))]
    (or (four-of-a-kind? hand)
        (= (filter not-one ranks) [2 2]))))

(defn straight? [hand]
  (let [n-ranks (count (set (vals (frequencies (map rank hand)))))
        ranks-hi (sort (map rank hand))
        ranks-lo (sort (replace {14 1} ranks-hi))
        is-straight? (fn [ranks] (= 5 (count (range (apply min ranks) (+ 1 (apply max ranks))))))]
    (and (= 1 n-ranks)
      (or (is-straight? ranks-hi)
          (is-straight? ranks-lo)))))

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
