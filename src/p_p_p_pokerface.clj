(ns p-p-p-pokerface)

(def rankmap {\A 14, \K 13, \Q 12, \J 11, \T 10})

(defn rank [card]
  (let [[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (rankmap rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn x-of-a-kind? [x hand]
  (let [ranks (map rank hand)
        rankfreqs (frequencies ranks)]
    (boolean (< (- x 1) (apply max (vals rankfreqs))))))

(defn pair? [hand]
  (x-of-a-kind? 2 hand))

(defn three-of-a-kind? [hand]
  (x-of-a-kind? 3 hand))

(defn four-of-a-kind? [hand]
  (x-of-a-kind? 4 hand))

(defn flush? [hand]
  (let [suits (map suit hand)
        suitfreqs (frequencies suits)]
    (boolean (< 4 (apply max (vals suitfreqs))))))

(defn full-house? [hand]
  (let [ranks (map rank hand)
        rankfreqs (sort(vals (frequencies ranks)))]
    (boolean (= rankfreqs [2 3]))))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        rankfreqs (sort(vals (frequencies ranks)))]
    (boolean (= (rest rankfreqs) [2 2]))))

(defn straight? [hand]
  (let [ranks (sort(map rank hand))
        minimum-rank (apply min ranks)]
    (boolean (or (= ranks
                (range minimum-rank (+ minimum-rank 5)))
                 (= ranks [2 3 4 5 14])))))

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



