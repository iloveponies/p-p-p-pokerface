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
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
