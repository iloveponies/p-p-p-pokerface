(ns p-p-p-pokerface)

(defn rank [card]
  (let [char->rank {\T 10 \J 11 \Q 12 \K 13 \A 14}
        [r _] card]
    (if (Character/isDigit r) (Integer/valueOf (str r)) (get char->rank r))))

(defn suit [card]
  (let [[_ s] card] (str s)))

(defn max-rank-count [hand]
  (let [ranks (map rank hand)]
    (apply max (vals (frequencies ranks)))))

(defn pair? [hand]
  (>= (max-rank-count hand) 2))

(defn three-of-a-kind? [hand]
  (>= (max-rank-count hand) 3))

(defn four-of-a-kind? [hand]
  (>= (max-rank-count hand) 4))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (== (count (frequencies suits)) 1)))

(defn full-house? [hand]
  (let [ranks (map rank hand)]
    (= [2 3] (sort (vals (frequencies ranks))))))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        rank-frequency (sort (vals (frequencies ranks)))]
    (or (= rank-frequency [1 2 2]) (= rank-frequency [1 4]))))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        replaced-ranks (sort (replace {14 1} ranks))]
    (or
      (= ranks (range (apply min ranks) (+ (apply max ranks) 1)))
      (= replaced-ranks (range (apply min replaced-ranks) (+ (apply max replaced-ranks) 1))))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1]
                  [two-pairs? 2] [three-of-a-kind? 3]
                  [straight? 4] [flush? 5]
                  [full-house? 6] [four-of-a-kind? 7]
                  [straight-flush? 8]}]
    (apply max(map second (filter (fn [checker] ((first checker) hand)) checkers)))))
