(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rnk _] card
        mp {\T 10
            \J 11
            \Q 12
            \K 13
            \A 14}]
    (if (Character/isDigit rnk)
      (Integer/valueOf (str rnk))
      (mp rnk))))

(defn suit [card]
  (let [[_ st] card]
    (str st)))

(defn pair? [hand]
  (let [ranks (map rank hand)]
    (= 2 (apply max (vals (frequencies ranks))))))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (= 3 (apply max (vals (frequencies ranks))))))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (= 4 (apply max (vals (frequencies ranks))))))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (= 5 (apply max (vals (frequencies suits))))))

(defn full-house? [hand]
  (let [ranks (map rank hand)]
    (= '(2 3) (sort (vals (frequencies ranks))))))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        sorted-ranks (sort (vals (frequencies ranks)))]
    (or (= '(1 2 2) sorted-ranks) (= '(1 4) sorted-ranks))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        sorted-ranks (sort (keys (frequencies ranks)))
        upd-ranks (sort (replace {14 1} sorted-ranks))
        strght? (fn [rnk] (= (range (first rnk) (+ (first rnk) 5)) rnk))]
    (or (strght? sorted-ranks) (strght? upd-ranks))))


(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [high-card? (fn [hand] true)
        checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
    (apply max (map second (filter (fn [a] ((first a) hand)) checkers)))))
