(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get {\A 14, \K 13, \Q 12,
            \J 11, \T 10} r))))

(defn suit [card]
  (let [[_ st] card]
    (str st)))

(defn rank-frequencies [hand]
  (let [ranks (map rank hand)]
    (vals (frequencies ranks))))

(defn pair? [hand]
  (== 2 (apply max (rank-frequencies hand))))

(defn three-of-a-kind? [hand]
  (== 3 (apply max (rank-frequencies hand))))

(defn four-of-a-kind? [hand]
  (== 4 (apply max (rank-frequencies hand))))

(defn flush? [hand]
  (let [suits (mapv suit hand)]
    (let [amount (vals (frequencies suits))]
      (== 5 (apply max amount)))))

(defn full-house? [hand]
  (let [freq (rank-frequencies hand)]
    (and
     (== 3 (apply max freq))
     (== 2 (apply min freq)))))

(defn two-pairs? [hand]
  (let [freq (sort > (rank-frequencies hand))]
    (let [bigger (drop-last freq)]
      (and
       (<= 2 (apply max bigger))
       (<= 2 (apply min bigger))))))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))]
    (let [ranks2 (sort (replace {14 1} ranks))]
      (or
       (= ranks (range (first ranks) (+ 1 (last ranks))))
       (= ranks2 (range (first ranks2) (+ 1 (last ranks2))))))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
  (let [possible (filter (fn [x] (= true ((first x) hand))) checkers)]
    (let [values (map second possible)]
      (apply max values)))))
