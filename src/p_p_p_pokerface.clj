(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card
        card-map {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get card-map r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn pair? [hand]
  (let [ranks (map rank hand)]
    (boolean (some (fn [x] (== x 2))
                   (vals (frequencies ranks))))))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (boolean
      (some (fn [x] (== x 3))
            (vals (frequencies ranks))))))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (boolean
      (some (fn [x] (== x 4))
            (vals (frequencies ranks))))))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (boolean
      (some (fn [x] (== x 5))
            (vals (frequencies suits))))))

(defn full-house? [hand]
  (and (pair? hand)
       (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (or (four-of-a-kind? hand)
      (let [ranks (map rank hand)]
        (==
          (count (filter
                   (fn [x] (== x 2))
                   (vals (frequencies ranks))))
          2))))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        r-ranks (sort (replace {14 1} ranks))
        max-rank (apply max ranks)
        min-rank (apply min ranks)
        r-max-rank (apply max r-ranks)
        r-min-rank (apply min r-ranks)]
    (if (< (count ranks) 5)
      false
      (or (= ranks (range min-rank (+ max-rank 1)))
          (= r-ranks (range r-min-rank (+ r-max-rank 1)))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand] true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max
           (map second (filter
                         (fn [[f _]]
                           (f hand))
                         checkers)))))
