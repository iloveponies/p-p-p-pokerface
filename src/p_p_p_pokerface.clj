(ns p-p-p-pokerface)

(defn rank [card]
  (let [[card-rank _] card]
    (def ranks {\T 10, \J 11
                \Q 12, \K 13
                \A 14})
      (if (contains? ranks card-rank)
        (get ranks card-rank)
        (let [card-rank-as-number (Integer/valueOf (str card-rank))]
          card-rank-as-number))))

(defn suit [card]
  (let [[_ sui] card]
    (str sui)))

(defn pair? [hand]
  (let [ranks (map rank hand)]
    (not= (count ranks) (count (distinct ranks)))))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (>= (apply max (vals (frequencies ranks))) 3)))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (>= (apply max (vals (frequencies ranks))) 4)))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (= (apply max (vals (frequencies suits))) 5)))

(defn full-house? [hand]
  (let [ranks (map rank hand)]
   (= [2 3] (sort (vals (frequencies ranks))))))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)]
    (or (four-of-a-kind? hand)
        ; wtf
        (= 1 (count
             (filter (fn [x] (let [[k v] x] (and (= 2 k) (= 2 v))))
             (frequencies (vals (frequencies ranks)))))))))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))]
    (cond (> (count (filter (fn [x] (> x 1)) (vals (frequencies ranks)))) 0)
      (boolean false)
    :else (if (and (= 2 (first ranks)) (= 14 (nth ranks(- (count ranks) 1))))
      (boolean true) (= 4 (- (nth ranks (- (count ranks) 1)) (first ranks)))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
   (let [filtered-checkers (filter (fn [pair] (let [[c v] pair] (= true (c hand)))) checkers)]
        (apply max (map second filtered-checkers)))))
